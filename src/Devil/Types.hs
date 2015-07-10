module Devil.Types where

import           Data.Aeson
import           Data.Map   (Map)
import           Data.Text  (Text)

data Config = Config {
     cfg_devilPath :: FilePath
    ,cfg_daemons   :: [DaemonGlobal]
    ,cfg_config    :: Map Text Value
} deriving (Show,Eq)

data DaemonGlobal = DaemonGlobal {
      daemon_name   :: Text
     ,daemon_config :: Map Text Value
} deriving (Show,Eq)

data DaemonLocal = DaemonLocal {
    daemon_entryPoint :: String
} deriving (Show,Eq)
