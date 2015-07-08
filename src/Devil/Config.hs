{-# LANGUAGE OverloadedStrings #-}
module Devil.Config (loadConfig) where

import Devil.Types
import Data.Yaml
import Control.Applicative
import Control.Monad

instance FromJSON Config where
    parseJSON (Object v) =
        Config <$> v .: "path"
               <*> v .: "enabled"
               <*> v .: "wakeup"
    parseJSON _ = mzero

instance FromJSON Daemon where
    parseJSON (Object v) =
        Daemon <$> v .:  "name"
               <*> v .:? "wakeup"
    parseJSON _ = mzero

loadConfig :: FilePath -> IO (Either String Config)
loadConfig = fmap (either (Left . show) Right) . decodeFileEither