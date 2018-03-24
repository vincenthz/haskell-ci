{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TextGen.Out
    ( Out
    , runOut
    , out
    , outNl
    , nl
    , withIndent
    , space
    , quoted
    , list
    ) where

import Control.Monad
import Foundation.Monad.State
import Basement.Compat.Natural
import Basement.Compat.Identity
import Data.List (isSuffixOf, intersperse)

data St = St
    { prevLines :: [String]
    , line      :: String
    , indentLvl :: Natural
    , indentBy  :: String
    }

modify :: (St -> St) -> Out ()
modify f = Out $ get >>= \st -> put (f st)

newtype Out a = Out { runOutRaw :: StateT St Identity a }
    deriving (Functor, Applicative, Monad)

runOut :: Out () -> String
runOut f = runIdentity (outputState . snd <$> runStateT (runOutRaw f) (St [] "" 0 "  "))
  where outputState st = unlines $ reverse (line st : prevLines st)

indent :: Out ()
indent = modify $ \st -> st { indentLvl = indentLvl st + 1 }

unIndent :: Out ()
unIndent = modify $ \st -> st { indentLvl = indentLvl st - 1 }

withIndent :: Out a -> Out a
withIndent f = do
    indent *> f <* unIndent

nl :: Out ()
nl = modify $ \st -> st
    { prevLines = line st : prevLines st
    , line      = ""
    }

out :: String -> Out ()
out s = modify $ \st -> st
    { line = if line st == ""
                then concat (replicate (fromIntegral $ indentLvl st) (indentBy st)) ++ s
                else line st ++ s
    }

space :: Out ()
space = modify $ \st -> st
    { line = if line st == ""
                then ""
                else if isSuffixOf " " (line st)
                        then line st
                        else line st ++ " "
    }

quoted :: Out () -> Out ()
quoted f = out "\"" >> f >> out "\""

outNl :: String -> Out ()
outNl s = out s >> nl

list :: [Out ()] -> Out ()
list l = sequence_ $ intersperse (out "," >> space) l
