{-# LANGUAGE DataKinds #-}
module Build where

import Config
import Utils
import Data.List
import Data.Function (on)
import Data.Maybe (catMaybes)

data OsType = Linux | OsX | Win
    deriving (Show,Eq)

data PackageFlag = PackageFlag String           -- ^ package name
                               [(String, Bool)] -- ^ list of (flag name + enabled)
    deriving (Show,Eq)

data BuildType = BuildWeeder
               | BuildHLint
               | BuildStack String OsType
    deriving (Show,Eq)

data Build = Build
    { buildName       :: String
    , buildResolver   :: String
    , buildFlags      :: [PackageFlag]
    , buildPackages   :: [String]
    , buildExtraDeps  :: [String]
    , buildUseHaddock :: Bool
    , buildAllowNewer :: Bool
    , buildTests      :: RunOpt
    , buildBenchs     :: RunOpt
    } deriving (Show,Eq)

data RunOpt = NotCompiled | JustCompile | RunCompile
    deriving (Show,Eq)

toRunOpt :: String -> Maybe RunOpt
toRunOpt "no"      = Just NotCompiled
toRunOpt "compile" = Just JustCompile
toRunOpt "run"     = Just RunCompile
toRunOpt _         = Nothing


makeBuildFromEnv :: C -> BuildEnv 'Resolved -> Build
makeBuildFromEnv c (BuildEnv compiler simple kvs) =
    let resolver = compilerToLts c compiler
        flags = map toPkgFlag $ groupBy ((==)  `on` fst) $ map parseFlag $ getFlags kvs
        extraPkgs = map snd $ filter ((==) Package . fst) kvs
        extraDeps = map snd $ filter ((==) ExtraDep . fst) kvs
        tests = maybe RunCompile id $ lastMaybe $ catMaybes $ map (toRunOpt . snd) $ filter ((==) Tests . fst) kvs
        benchs = maybe JustCompile id $ lastMaybe $ catMaybes $ map (toRunOpt . snd) $ filter ((==) Benchs . fst) kvs
        noHaddock = elem NoHaddock simple
     in Build { buildName        = compiler
              , buildResolver    = resolver
              , buildFlags       = flags
              , buildPackages    = packages c ++ extraPkgs
              , buildExtraDeps   = extraDeps
              , buildUseHaddock  = not noHaddock
              , buildAllowNewer  = elem AllowNewer simple
              , buildTests       = tests
              , buildBenchs      = benchs
              }
  where getFlags = map snd . filter ((==) Flag . fst)
        parseFlag x = case splitChar ':' x of
                            Nothing -> error ("cannot parse flag " ++ x ++ " should be of the form \"packagename:(-)flagname\"")
                            Just (p,f) | isPrefixOf "-" f -> (p, (tail f, False))
                                       | otherwise        -> (p, (f     , True))
        toPkgFlag l = PackageFlag (head $ map fst l) (map snd l)
