
haskell package CI generator
============================

Generate a consistent set of CI files for haskell packages repository
with first class support for stackage LTS, stack, and optional tools
like weeder, hlint.


## TODO:

* Add appveyor support
* Add coverall

## Getting started

* Create a new .haskell-ci file: `haskell-ci generate`
* Create a travis file related to the .haskell-ci file: `haskell-ci travis > .travis.yml`

## Config

haskell-ci looks for a `.haskell-ci` to translate into CI files
for travis and in the future appveyor

Main options are

* `compiler`: name for a compiler and its associated lts
* `option`: option alias for builds
* `build`: define a build with a compiler and some option
* `package`: the package that need to be build
* `hlint`: whether hlint is supported (enabled, disabled, allowed-failure)
* `weeder`: whether weeder is supported (enabled, disabled, allowed-failure)
* `coverall`: whether coverall is supported (enabled, disabled, allowed-failure)

Example of a simple `.haskell-ci` :

```
# compiler supported and their equivalent LTS
compiler: ghc-7.8 lts-2.22
compiler: ghc-7.10 lts-6.35
compiler: ghc-8.0 lts-9.21
compiler: ghc-8.2 lts-10.4

# options
option: myflag flag=mypackage:something

# builds 
build: ghc-7.8 myflag
build: ghc-8.2
build: ghc-7.10
build: ghc-8.0 myflag package=extra-package/
build: ghc-8.0 os=osx

# packages
package: '.'
package: other-pkg/

# extra builds
hlint: allowed-failure
weeder: allowed-failure
coverall: false
```
