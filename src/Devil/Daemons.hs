{-# LANGUAGE DeriveDataTypeable #-}
module Devil.Daemons (runDaemons) where

import           Devil.Types
import qualified Devil.Log as Log
import           System.FilePath
import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Control.Concurrent
import           Control.Conditional
import           Data.Function
import           Data.Typeable
import qualified Data.ByteString.Char8 as BS
import qualified Scripting.Lua as Lua

data DaemonException = LuaError String String
    deriving (Show,Typeable)
instance Exception DaemonException

entryPoint = "init.lua"
entryPoint :: FilePath

runDaemons :: Config -> IO ()
runDaemons (Config {
                cfg_daemonsPath = dir
               ,cfg_daemons     = daemons
               ,cfg_wakeupDelay = delay
           })= do
    Log.info "Starting daemons..."
    refs <- forM daemons $ \(Daemon name wakeup) -> do
        Log.info (":: " ++ name)
        runDaemon (dir </> name </> entryPoint) name (maybe delay id wakeup)
    forM_ refs readMVar -- waiting for threads



runDaemon :: FilePath -> String -> Int -> IO (MVar ())
runDaemon path daemonName wakeupDelay = do
    ret <- newEmptyMVar
    forkIO $ bracket
        Lua.newstate
        (\l -> do
            Lua.close l
            putMVar ret ())
        (\l -> do
            Lua.openlibs l
            Lua.loadfile l path
            Lua.call l 0 0
            forever $ do
                daemon l daemonName
                threadDelay (wakeupDelay * 10^6)
        )
    return ret

daemon :: Lua.LuaState -> String -> IO ()
daemon l daemonName = do
        Lua.getglobal l "cond"
        ifM ((==0) <$> Lua.pcall l 0 1 0)
            (return ())
            (do
                err <- fmap BS.unpack (Lua.tostring l (-1))
                throw (LuaError daemonName err)
            )
        ifM (( Lua.isnil l (-1)) <||>
             ((Lua.isboolean l (-1) <&&> (notM (Lua.toboolean l (-1))))))
                -- if the return value is `nil` or `false` 
                (Lua.pop l (-1))
                -- else
                (do
                    x <- ifM (Lua.isboolean l (-1))
                            (Just <$> Lua.ref l Lua.registryindex)
                            (return Nothing)
                    Lua.pop l (-1)
                    Lua.getglobal l "action"
                    ifM ((==0) <$> case x of
                                    Nothing -> Lua.pcall l 0 0 0
                                    Just r  -> do
                                        Lua.rawgeti l Lua.registryindex r
                                        x <- Lua.pcall l 1 0 0
                                        Lua.unref   l Lua.registryindex r
                                        return x)
                        (return ())
                        (do
                            err <- fmap BS.unpack (Lua.tostring l (-1))
                            throw (LuaError daemonName err)
                        )
                    return ()
                )