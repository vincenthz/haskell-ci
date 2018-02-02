module Build where

data OsType = Linux | OsX | Win
    deriving (Show,Eq)

data PackageFlag = PackageFlag String           -- ^ package name
                               [(String, Bool)] -- ^ list of (flag name + enabled)

data BuildType = BuildWeeder
               | BuildHLint
               | BuildStack String OsType
    deriving (Show,Eq)
