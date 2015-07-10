{-# LANGUAGE OverloadedStrings #-}
module Devil.Log (info) where

import           Data.Monoid
import           Data.Text    (Text)
import qualified Data.Text.IO as T.IO

info :: Text -> IO ()
info str = do
    T.IO.putStrLn $ "[Devil] " <> str
