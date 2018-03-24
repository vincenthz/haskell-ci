module Stack where

import           Build
import           Resolver
import qualified Yaml as Y

stackYaml :: Build -> String
stackYaml build = Y.toString $ Y.dict $
    resolverToYaml resolver ++
    [ (Y.key "packages", Y.list $ map Y.string (buildPackages build))
    , (Y.key "extra-deps", Y.list $ map writeDependency (buildExtraDeps build))
    , (Y.key "flags", Y.dict (map toPkgFlag $ buildFlags build))
    ] ++ (if buildAllowNewer build then [(Y.key "allow-newer", Y.string "true")] else [])
  where
    resolver = classifyResolver (buildResolver build)
    toPkgFlag (PackageFlag pkg flags) =
        (Y.key pkg, Y.dict $ map (\(flagName, enabled) -> (Y.key flagName, Y.string $ if enabled then "true" else "false")) flags )

writeDependency (DependencySimple s) = Y.string s
writeDependency (DependencyGit (GitDependency loc mcom subdirs)) =
    Y.dict $ [ (Y.key "git", Y.qstring loc) ]
          ++ (maybe [] (\c -> [(Y.key "commit", Y.qstring c)]) mcom)
          ++ (if null subdirs then [] else [ (Y.key "subdirs", Y.list $ map Y.qstring subdirs) ])
