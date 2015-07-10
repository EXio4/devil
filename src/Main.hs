{-# LANGUAGE LambdaCase, NamedFieldPuns #-}
module Main (main) where

import           Devil.Config
import           Devil.Daemons
import           Options.Applicative

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
main = go =<< execParser params where
        go (Params{configFile}) =
            loadConfig configFile >>= \case
                Left err -> do
                    putStrLn "Error loading config"
                    putStrLn ("\t" ++ show err)
                Right cfg ->
                    runDaemons cfg
