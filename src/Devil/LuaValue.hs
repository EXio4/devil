{-# LANGUAGE LambdaCase #-}
module Devil.LuaValue (
   LuaValue(..)
  ,pop
  ,push
) where

import           Control.Applicative
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Scripting.Lua         (LuaState, StackValue)
import qualified Scripting.Lua         as Lua

instance StackValue Text where
    push l   = Lua.push l . T.encodeUtf8
    peek l n = fmap T.decodeUtf8 <$> Lua.peek l n
    valuetype _ = Lua.valuetype (undefined :: ByteString)


data LuaValue = LuaInteger !Int
              | LuaString  !Text
              | LuaBoolean !Bool
              | LuaNil


pop :: LuaState -> Int -> IO (Maybe LuaValue)
pop l n =
  (\a b c d -> a <|> b <|> c <|> d)
        <$> (fmap LuaInteger <$> (Lua.peek l n :: IO (Maybe Int)  ))
        <*> (fmap LuaString  <$> (Lua.peek l n :: IO (Maybe Text) ))
        <*> (fmap LuaBoolean <$> (Lua.peek l n :: IO (Maybe Bool) ))
        <*> (    (LuaNil <$) <$> (Lua.peek l n :: IO (Maybe ())   ))

push :: LuaState -> LuaValue -> IO ()
push l = \case
      LuaInteger n -> Lua.push l n
      LuaString  s -> Lua.push l s
      LuaBoolean b -> Lua.push l b
      LuaNil       -> Lua.push l ()
