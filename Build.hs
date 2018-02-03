module Build where

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
