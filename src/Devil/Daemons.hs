{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Devil.Daemons (runDaemons) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Conditional
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Function
import           Data.IORef
import           Data.Map              (Map)
import qualified Data.Map.Strict       as M
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Typeable
import           Data.Yaml             (ParseException)
import qualified Data.Yaml             as YAML
import           Devil.Config
import qualified Devil.Log             as Log
import           Devil.Types
import           Foreign.C.Types
import           Scripting.Lua         (LuaState, StackValue (..))
import qualified Scripting.Lua         as Lua
import           System.FilePath
import qualified Control.Concurrent.Chan

type Environment = Map Text Value

data DaemonException = LuaError           !Text
                     | LuaException       !Text
                     | LuaReRegister
                     | ConfigError        !ParseException
                     | UnregisteredDaemon
    deriving (Show,Typeable)
instance Exception DaemonException

instance StackValue Text where
    push l   = push l . T.encodeUtf8
    peek l n = fmap (fmap T.decodeUtf8) $ peek l n
    valuetype _ = valuetype (undefined :: ByteString)

(typDummy,typBusyWait) = (0,1)

runDaemons :: Config -> IO ()
runDaemons (Config {
                cfg_devilPath = dir
               ,cfg_daemons   = daemons
               ,cfg_config    = global_cfg
           })= do
    Log.info "Starting daemons..."
    refs <- forM daemons $ \(DaemonGlobal name local_cfg) -> do
        Log.loadingData name
        runDaemon dir name (local_cfg `M.union` global_cfg)
    forM_ refs readMVar -- waiting for threads


wrap :: StackValue a => (Text -> IO (Maybe a)) -> LuaState -> IO CInt
wrap f l =
    ifM (notM (Lua.isstring l 1)) (
        return (-1)
    ) $ do
        str <- fmap T.decodeUtf8 $ Lua.tostring l 1
        Lua.pop l 1
        x <- f str
        case x of
            Nothing -> Lua.push l ()
            Just v  -> Lua.push l v
        return 1

runDaemon :: FilePath -> Text -> Environment -> IO (MVar ())
runDaemon path daemonName environment = do
    let daemon_path = path </> "daemons" </> T.unpack daemonName
    ret <- newEmptyMVar
    let handlers =
            [Handler (\case
                LuaError      why      -> Log.error_d daemonName
                          ("Lua Error: " <> why)
                LuaException  why      -> Log.error_d daemonName
                          ("Lua exception: " <> why)
                LuaReRegister          -> Log.error_d daemonName
                          "Daemon re-registered itself"
                ConfigError   parseExc -> Log.error_d daemonName
                          ("Config Error" <> T.pack (show parseExc))
                UnregisteredDaemon     -> Log.error_d daemonName
                          ("Daemon didn't register into the system")
            ),
            Handler (\(e :: SomeException) ->
                  Log.error_d daemonName ("UnknownException :: " <> T.pack (show e))
            )]
    forkIO . flip catches handlers $
        bracket
        Lua.newstate
        (\l -> do
            Lua.close l
            putMVar ret ())
        (\l -> do
            ref <- newIORef Nothing
            (DaemonLocal entryPoint) <-
                            YAML.decodeFileEither (daemon_path </> "daemon.conf") >>=
                            either (throw .  ConfigError) return
            Lua.openlibs l
            Lua.registerrawhsfunction l "_internal_get_integer"     (wrap $ getInteger environment)
            Lua.registerrawhsfunction l "_internal_get_string"      (wrap $ getString  environment)
            Lua.registerrawhsfunction l "_internal_log_info"        (wrapLog $ Log.info_d daemonName)
            Lua.registerrawhsfunction l "_internal_register_daemon" $
                    fmap fromIntegral . registerDaemon ref daemonName environment
            Lua.loadfile l (path </> "common" </> "api.lua")
            Log.info (daemonName <> " :: Loading (common) api")
            pcall' l daemonName 0 0 0
            Log.info (daemonName <> " :: Loading daemon")
            Lua.loadfile l (daemon_path </> entryPoint)
            pcall' l daemonName 0 0 0
            fn <- maybe (throw UnregisteredDaemon) return =<< readIORef ref
            case fn of
                    DaemonDummy -> do
                         Log.info "(dummy setup, used for checking initializing core stuff was possible)"
                         return ()
                    DaemonBusyWait cfg -> busyWait l daemonName cfg
        )
    return ret


wrapLog :: (Text -> IO ()) -> LuaState -> IO CInt
wrapLog f l = do
  f =<< (T.decodeUtf8 <$> Lua.tostring l 1)
  Lua.pop l 1
  return 0

getInteger :: Environment -> Text -> IO (Maybe Int)
getInteger m txt = return x
    where x = do
                Number v <- M.lookup txt m
                return $ round v
getString  :: Environment -> Text -> IO (Maybe Text)
getString m txt = return x
    where x = do
                String v <- M.lookup txt m
                return v

registerDaemon :: IORef (Maybe Daemon) -> Text -> Environment-> LuaState -> IO Int
registerDaemon ref daemonName env l = do
    -- we make sure that this is the `first` valid call to register_daemon
    maybe (return ()) (const $ throw LuaReRegister) =<< readIORef ref

    {- we do no error checking, this function should only be called internally
       after all invariants were "checked", this leads to some kind of fragile code
       but the other way I could think of was to just do the checking here
       (which would, indeed, be nicer, I have to learn to use the lua c api first though)
    -}
    (typ :: Maybe Int) <- Lua.peek l 1
    case typ of
      Nothing  -> return (-1)
      Just typ ->
            if | typ == typDummy -> do
                    writeIORef ref (Just DaemonDummy)
                    Lua.settop l 0
                    return 0
               | typ == typBusyWait -> do
                   x <- getInteger env "wakeup"
                   Lua.getfield l 2 "condition"
                   cond   <- Lua.ref l Lua.registryindex
                   Lua.getfield l 2 "action"
                   action <- Lua.ref l Lua.registryindex
                   Lua.settop l 0
                   case x of
                        Just wakeupDelay -> do
                            writeIORef ref (Just (DaemonBusyWait (BusyWait wakeupDelay cond action)))
                            return 0
                        Nothing          ->
                            return (-1)
               | otherwise -> throw (LuaError "[internal] given `register_daemon` invalid type of daemon")

type LuaRefFunction = Int

data Daemon
    = DaemonBusyWait !BusyWait
    | DaemonDummy

data BusyWait
    = BusyWait
        !Int            -- ^ wake up delay (in seconds)
        !LuaRefFunction -- ^ condition to check
        !LuaRefFunction -- ^ action to take place when the condition is true

sleepS :: Int -> IO ()
sleepS n = mapM_ (const (threadDelay (10^6))) [1..n]

busyWait :: LuaState -> Text -> BusyWait -> IO ()
busyWait l daemonName (BusyWait wakeupDelay cond action) =
            (forever . (>> sleepS wakeupDelay)) $ do
                Lua.rawgeti l Lua.registryindex cond
                pcall' l daemonName 0 1 0
                whenM (Lua.isboolean l (-1) <&&> Lua.toboolean l (-1)) $ do
                    Lua.rawgeti l Lua.registryindex action
                    pcall' l daemonName 0 0 0
                Lua.pop l 1

pcall' :: LuaState -> Text -> Int -> Int -> Int -> IO ()
pcall' l daemonName x y z =
        whenM ((/=0) <$> Lua.pcall l x y z) luaError
    where luaError = throw . f =<< fmap T.decodeUtf8 (Lua.tostring l (-1))
          prefix = "'LUA_EXC_"
          {- this string hackery is done because throwing an exceptiom from within a lua pcall
             pretty much breaks the exception handler
             (should check other ways to do this, maybe one that doesn't suck)
          -}
          f x = case fmap (T.drop (T.length prefix)) $ T.breakOn prefix x of
            (x, "") -> LuaError x
            (t, r)  -> LuaException (t <> r)
