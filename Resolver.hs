module Resolver where

import qualified Yaml as Y
import           Data.List

data Resolver = ResolverStackage String
              | ResolverOther    ResolverParameters

data ResolverParameters = ResolverParameters String String [Target]

data Target = Target String String String String

resolverToYaml :: Resolver -> [(Y.Key, Y.Content)]
resolverToYaml (ResolverStackage s) =
    [(Y.key "resolver", Y.string s)]
resolverToYaml (ResolverOther (ResolverParameters n compiler targets)) =
    [ targetInfo targets
    , (Y.key "resolver", Y.string ("ghc-" ++ compiler))
    , (Y.key "compiler", Y.string ("ghc-" ++ compiler))
    , (Y.key "compiler-check", Y.string "match-exact")
    ]
  where
    targetInfo :: [Target] -> (Y.Key, Y.Content)
    targetInfo setups =
        (Y.key "setup-info", Y.dict [ (Y.key "ghc", Y.dict $ map setupToYaml setups ) ])
      where setupToYaml (Target target compiler url sha256) =
                let innerDict = Y.dict [ (Y.key "url", Y.qstring url), (Y.key "sha256", Y.qstring sha256) ]
                 in (Y.qkey target, Y.dict [ (Y.qkey compiler, innerDict) ])

ghc84alpha2 :: ResolverParameters
ghc84alpha2 = ResolverParameters "ghc-8.4-alpha2" compilerVer targets
  where
    url s = "https://downloads.haskell.org/~ghc/8.4.1-alpha2/" ++ s
    compilerVer = "8.4.0.20180118"
    targets = 
        [ Target "linux32-nopie" compilerVer (url "ghc-8.4.0.20180118-i386-deb8-linux.tar.xz") "be1a3b5de9f671199533d22f2810d9b62c6392b32b39833cd384a094566703c6"
        , Target "windows32" compilerVer (url "ghc-8.4.0.20180118-i386-unknown-mingw32.tar.xz") "3f4b9291ad35d89ca7b3561312a4329545aedceb5c4c8c5c4cf01550037376a1"
        , Target "linux64" compilerVer (url "ghc-8.4.0.20180118-x86_64-deb8-linux.tar.xz") "55b54bce14661c19288c3413b8fab95d2b7fae407986323c7f0b6a732bec6a38"
        , Target "linux64-tinfo" compilerVer (url "ghc-8.4.0.20180118-x86_64-deb8-linux.tar.xz") "55b54bce14661c19288c3413b8fab95d2b7fae407986323c7f0b6a732bec6a38"
        , Target "linux64-tinfo6" compilerVer (url "ghc-8.4.0.20180118-x86_64-fedora27-linux.tar.xz") "47c7e4350c9560f984bde75b243aa10c91e37494152d87d20f84fcee857338ef"
        , Target "linux64-tinfo-nopie" compilerVer (url "ghc-8.4.0.20180118-x86_64-fedora27-linux.tar.xz") "47c7e4350c9560f984bde75b243aa10c91e37494152d87d20f84fcee857338ef"
        , Target "linux64-nopie" compilerVer (url "ghc-8.4.0.20180118-x86_64-deb8-linux.tar.xz") "55b54bce14661c19288c3413b8fab95d2b7fae407986323c7f0b6a732bec6a38"
        , Target "windows64" compilerVer (url "ghc-8.4.0.20180118-x86_64-unknown-mingw32.tar.xz") "93dd7f80e3c645b79a91f3023046144ec88927961a3443019034e2893de43752"
        , Target "macosx" compilerVer (url "ghc-8.4.0.20180118-x86_64-apple-darwin.tar.xz") "b3119b255ab3d1a09fcf9919bddbbe2cd77f9175de14e4b23f20b40abe5edea1" 
        ]

ghc84rc1 :: ResolverParameters
ghc84rc1 = ResolverParameters "ghc-8.4-rc1" compilerVer targets
  where
    url s = "https://downloads.haskell.org/~ghc/8.4.1-rc1/" ++ s
    compilerVer = "8.4.0.20180224"
    targets =
        [ Target "windows64" compilerVer (url "ghc-8.4.0.20180224-x86_64-unknown-mingw32.tar.xz") "7d125b3bbaa2ec4022215787a259a41bf944ec02736c9555f2a19432eef21cad"
        , Target "linux64" compilerVer (url "ghc-8.4.0.20180224-x86_64-deb8-linux.tar.xz") "a83b57a600b513f689716e98b53badfb2e2aa1117bff7c5bab12c07854eecb07"
        , Target "macosx" compilerVer (url "ghc-8.4.0.20180224-x86_64-apple-darwin.tar.xz") "4cf1afc5c6672e4628ba84c775a081fc4cf08bddd07f30fcc57d909fc5939c06"
        , Target "windows32" compilerVer (url "ghc-8.4.0.20180224-i386-unknown-mingw32.tar.xz") "28799a8ceee197a050fa1ef0740a78b01bf93b13f5a7b8e7ef5061b231c270dc"
        , Target "linux32-nopie" compilerVer (url "ghc-8.4.0.20180224-i386-deb8-linux.tar.xz") "ce2f7583c64b700b22281a488e9a950ee50ec756a83520e3360748f67e14bfa2"
        , Target "linux64-nopie" compilerVer (url "ghc-8.4.0.20180224-x86_64-deb8-linux.tar.xz") "a83b57a600b513f689716e98b53badfb2e2aa1117bff7c5bab12c07854eecb07"
        , Target "linux64-tinfo-nopie" compilerVer (url "ghc-8.4.0.20180224-x86_64-deb8-linux.tar.xz") "a83b57a600b513f689716e98b53badfb2e2aa1117bff7c5bab12c07854eecb07"
        , Target "linux64-tinfo6" compilerVer (url "ghc-8.4.0.20180224-x86_64-fedora27-linux.tar.xz") "34240b8055e81f78a58becd1351ce03235e1bb3fd14c419a7e97631514a780ad"
        ]


extraResolvers :: [ResolverParameters]
extraResolvers = [ ghc84alpha2, ghc84rc1 ]

findExtraResolver :: String -> Maybe ResolverParameters
findExtraResolver name = find (\(ResolverParameters n _ _) -> n == name) extraResolvers

classifyResolver :: String -> Resolver
classifyResolver s = maybe (ResolverStackage s) ResolverOther $ findExtraResolver s
