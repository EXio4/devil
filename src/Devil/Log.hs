{-# LANGUAGE OverloadedStrings #-}
module Devil.Log (info) where

import qualified Data.Text.IO as T.IO
import qualified Data.Text    as T
import           Data.Text       (Text)
import           Data.Monoid

info :: Text -> IO ()
info str = do
    T.IO.putStrLn $ "[Devil] " <> str
