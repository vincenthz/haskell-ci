module Yaml
    ( Content
    , Key
    , toString
    , dict
    , string
    , key
    , list
    ) where

import Data.List

newtype Content = Content { toString :: String }

newtype Key = Key String

key :: String -> Key
key = Key -- TODO escape

dict :: [(Key, Content)] -> Content
dict [] = Content "{}"
dict kv = Content ("{ " ++ intercalate ", " (map toKv kv) ++ " }")
  where toKv (Key k,v) = k ++ ": " ++ toString v

string :: String -> Content
string = Content

list :: [Content] -> Content
list [] = Content "[]"
list l  = Content ("[ " ++ intercalate ", " (map toString l) ++ " ]")
