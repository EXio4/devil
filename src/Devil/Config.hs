{-# LANGUAGE OverloadedStrings #-}
module Devil.Config (loadConfig) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Map.Strict     as M
import           Data.Yaml
import           Devil.Types

instance FromJSON Config where
    parseJSON (Object v) =
        Config <$> v .: "path"
               <*> v .: "enabled"
               <*> fmap (maybe M.empty id) (v .:? "config")
    parseJSON _ = mzero

instance FromJSON DaemonGlobal where
    parseJSON (Object v) =
        DaemonGlobal <$> v .: "name"
                     <*> fmap (maybe M.empty id) (v .:? "config")
    parseJSON _ = mzero

instance FromJSON DaemonLocal where
    parseJSON (Object v) =
        DaemonLocal <$> v .: "entry_point"
    parseJSON _ = mzero

loadConfig :: FilePath -> IO (Either ParseException Config)
loadConfig = decodeFileEither
