{-# LANGUAGE LambdaCase, NamedFieldPuns, OverloadedStrings #-}
module Main (main) where

import           Devil.Config
import           Devil.Daemons
import           Options.Applicative
import qualified Data.Text as T
import qualified Devil.Log as Log

data Params = Params {
     configFile :: String
} deriving (Show,Eq)

params' :: Parser Params
params'
    =   Params
    <$> strOption
            (  long  "config"
            <> short 'c'
            <> metavar "CONFIG"
            <> help "Config file")

params :: ParserInfo Params
params = info (helper <*> params')
            (  progDesc "Small (and silly!) `daemon` manager"
            )

main :: IO ()
main = do
  Log.logThread
  go =<< execParser params where
        go (Params{configFile}) =
            loadConfig configFile >>= \case
                Left err -> do
                    Log.error_d "CONFIG" (T.pack $ show err)
                Right cfg ->
                    runDaemons cfg
