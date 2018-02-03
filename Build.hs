{-# LANGUAGE DataKinds #-}
module Build where

import Config
import Data.List
import Data.Function (on)

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
    } deriving (Show,Eq)

makeBuildFromEnv :: C -> BuildEnv 'Resolved -> Build
makeBuildFromEnv c (BuildEnv compiler simple kvs) =
    let resolver = compilerToLts c compiler
        flags = map toPkgFlag $ groupBy ((==)  `on` fst) $ map parseFlag $ getFlags kvs
        extraPkgs = map snd $ filter ((==) "package" . fst) kvs
        extraDeps = map snd $ filter ((==) "extradep" . fst) kvs
        noHaddock = elem "nohaddock" simple
     in Build { buildName        = compiler
              , buildResolver    = resolver
              , buildFlags       = flags
              , buildPackages    = packages c ++ extraPkgs
              , buildExtraDeps   = extraDeps
              , buildUseHaddock  = not noHaddock
              }
  where getFlags = map snd . filter ((==) "flag" . fst)
        parseFlag x = case splitChar ':' x of
                            Nothing -> error ("cannot parse flag " ++ x ++ " should be of the form \"packagename:(-)flagname\"")
                            Just (p,f) | isPrefixOf "-" f -> (p, (f, False))
                                       | otherwise        -> (p, (f, True))
        toPkgFlag l = PackageFlag (head $ map fst l) (map snd l)

