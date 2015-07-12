{-# LANGUAGE OverloadedStrings #-}
module Devil.Log (
     logThread

    ,loadingData
    ,loadingData_d

    ,debug
    ,debug_d
    ,info
    ,info_d
    ,warning
    ,warning_d
    ,error
    ,error_d
) where

import           Prelude hiding (error)
import           Data.Monoid
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO
import           Control.Applicative
import           Control.Monad
import           System.IO.Unsafe
import           Control.Concurrent
import           Control.Concurrent.Chan
import qualified Data.Time            as Time
import qualified Data.Time.Format     as Locale
import           System.Console.ANSI

data Err = S Text | C Color

data Level = DEBUG | FILE | INFO | WARNING | ERROR
    deriving (Show,Eq,Enum)

{-# NOINLINE chan #-}
chan :: Chan [Err]
chan = unsafePerformIO $ newChan

lC :: Level -> Err
lC ERROR   = C Red
lC WARNING = C Yellow
lC INFO    = C Green
lC DEBUG   = C Cyan
lC _       = C White

getAccLevel :: Level
appName :: Text

getAccLevel = DEBUG
appName     = "Devil"

tzformat_d,tzformat_t :: String
tzformat_d = "%d/%m"
tzformat_t = "%T"

logThread :: IO ()
logThread = () <$ (forkIO . forever $ readChan chan >>= putLn)

message :: Text -> Level -> [Err] -> IO ()
message scope lvl str = do
    let acc = getAccLevel
    let getTime format = Time.formatTime Locale.defaultTimeLocale format <$> Time.getCurrentTime
    time1 <- T.pack <$> getTime tzformat_d
    time2 <- T.pack <$> getTime tzformat_t
    when (fromEnum lvl >= fromEnum acc) $
       writeChan chan
             ([C Red, S "(", C Magenta, S time1, C Yellow
              , S "[", C Magenta, S time2, C Yellow, S "]"
              , C Red, S "|", C Magenta, S scope, C Red, S ") "
              , lC lvl, S (T.pack (show lvl)), C White, S "\t"] <> str)
    -- should ignore this for now, we're doing proper exception handling n stuff
    when (lvl == ERROR) $ return ()

putLn :: [Err] -> IO ()
putLn xs = mapM_ prn xs >> setSGR [Reset] >> putStrLn ""
    where prn (S x) = T.IO.putStr x
          prn (C x) = setSGR [SetColor Foreground Vivid x]

debug       = debug_d       appName
loadingData = loadingData_d appName
info        = info_d        appName
warning     = warning_d     appName
error       = error_d       appName


debug_d       d x = message d DEBUG   [S x]
loadingData_d d x = message d FILE [C Green, S "\t-> ", C Cyan, S x]
info_d        d x = message d INFO    [S x]
warning_d     d x = message d WARNING [S x]
error_d       d x = message d ERROR   [S x]
