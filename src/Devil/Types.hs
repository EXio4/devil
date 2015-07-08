module Devil.Types where

data Config = Config {
     cfg_daemonsPath  :: FilePath
    ,cfg_daemons      :: [Daemon]
    ,cfg_wakeupDelay  :: Int
} deriving (Show,Eq)

data Daemon = Daemon {
      daemon_name    :: String
     ,daemon_wakeup  :: Maybe Int
} deriving (Show,Eq)
