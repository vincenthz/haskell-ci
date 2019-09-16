module Yaml
    ( Content
    , Key
    , toString
    , dict
    , string
    , qstring
    , key
    , qkey
    , list
    ) where

import Data.List

newtype Content = Content { toString :: String }

newtype Key = Key String

key :: String -> Key
key = Key -- TODO escape

qkey :: String -> Key
qkey s = -- TODO escape
    Key ("\"" ++ s ++ "\"")

dict :: [(Key, Content)] -> Content
dict [] = Content "{}"
dict kv = Content ("{ " ++ intercalate ", " (map toKv kv) ++ " }")
  where toKv (Key k,v) = k ++ ": " ++ toString v

string :: String -> Content
string s
   | needEscape = qstring s
   | otherwise  = Content s
  where
    needEscape = maybe False (const True) $ find (flip elem dict) s
    dict = [':']

qstring :: String -> Content
qstring s = Content ("\"" ++ s ++ "\"")

list :: [Content] -> Content
list [] = Content "[]"
list l  = Content ("[ " ++ intercalate ", " (map toString l) ++ " ]")
