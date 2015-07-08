module Devil.Log (info) where

import Prelude hiding (error)

info :: String -> IO ()
info str = do
    putStrLn $ "[Devil] " ++ str
