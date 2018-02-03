{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Config where

import Data.Char
import Data.List
import Data.Either

type SimpleOption = String
type KvOption = (String, String)
type CompilerName = String
type BuildName = String

data C = C
    { compilers :: [(CompilerName, String)]
    , builds    :: [BuildEnv 'Unresolved]
    , options   :: [ (String, ([SimpleOption], [KvOption])) ]
    , packages  :: [String]
    , hlint     :: Enabled
    , weeder    :: Enabled
    , coverall  :: Enabled
    }
    deriving (Show,Eq)

data Enabled = Enabled | Disabled | AllowedFailure
    deriving (Show,Eq)

data BuildEnvStat = Resolved | Unresolved

data BuildEnv (stat :: BuildEnvStat) = BuildEnv BuildName [SimpleOption] [KvOption]
    deriving (Show,Eq)

compilerToLts :: C -> String -> String
compilerToLts c s =
    maybe (error "cannot find compiler definition for " ++ s) id $ lookup s (compilers c)

parse :: String -> C
parse = foldl' mkC (C [] [] [] [] Disabled Disabled Disabled)
      . filter (\s -> not (null s || all isSpace s))
      . map stripComment
      . lines
  where
    stripComment :: String -> String
    stripComment []      = []
    stripComment ('#':_) = []
    stripComment (x:xs)  = x:stripComment xs

    mkC acc l = case words l of
        ("build:":name:opts)          -> acc { builds = builds acc ++ [parseBuild name opts] }
        ("compiler:":compiler:lts:[]) -> acc { compilers = compilers acc ++ [(compiler, lts)] }
        ("option:":optAlias:r)        -> acc { options = options acc ++ [(optAlias, parseOpts r)] }
        ("package:":pkg:[])           -> acc { packages = packages acc ++ [pkg] }
        ("weeder:":o:[])              -> acc { weeder = parseEnabled o }
        ("hlint:":o:[])               -> acc { hlint = parseEnabled o }
        ("coverall:":o:[])            -> acc { coverall = parseEnabled o }
        _                             -> error ("unknown line : " ++ show l)

    parseBuild buildName opts =
        let (simple, kvs) = parseOpts opts
         in BuildEnv buildName simple kvs

    parseOpts = partitionEithers . map parseOpt
    parseOpt o = case splitChar '=' o of
                    Nothing   -> Left o
                    Just pair -> Right pair
        
    parseEnabled "enabled" = Enabled
    parseEnabled "1" = Enabled
    parseEnabled "True" = Enabled
    parseEnabled "disabled" = Disabled

    parseEnabled "0" = Disabled
    parseEnabled "false" = Disabled
    parseEnabled "allowed-failure" = AllowedFailure
    parseEnabled s = error ("parsing enable flag: unknown " ++ show s)

-- | Try to split a string
-- 
-- > splitChar '=' "abc=def"
-- Just ("abc", "def")
splitChar :: Char -> String -> Maybe (String, String)
splitChar c s = case findIndex (== c) s of
                    Nothing  -> Nothing
                    Just idx -> let (l,r) = splitAt idx s in Just (l, tail r)

