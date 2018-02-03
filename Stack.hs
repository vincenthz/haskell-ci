module Stack where

import           Build
import           Resolver
import qualified Yaml as Y

stackYaml :: Build -> String
stackYaml build = Y.toString $ Y.dict $
    resolverToYaml resolver ++
    [ (Y.key "packages", Y.list $ map Y.string (buildPackages build))
    , (Y.key "extra-deps", Y.list $ map Y.string (buildExtraDeps build))
    , (Y.key "flags", Y.dict (map toPkgFlag $ buildFlags build))
    ]
  where
    resolver = classifyResolver (buildResolver build)
    toPkgFlag (PackageFlag pkg flags) =
        (Y.key pkg, Y.dict $ map (\(flagName, enabled) -> (Y.key flagName, Y.string $ if enabled then "true" else "false")) flags )
