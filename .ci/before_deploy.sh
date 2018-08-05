# This script takes care of building your crate and packaging it for release

set -ex

main() {
    local src=$(pwd) \
          stage=

    case $TRAVIS_OS_NAME in
        linux)
            stage=$(mktemp -d)
            ;;
        osx)
            stage=$(mktemp -d -t tmp)
            ;;
    esac

    stack install
    INSTALLED_ROOT=$(stack path --local-install-root)

    echo $TRAVIS_OS_NAME
    cp $INSTALLED_ROOT/bin
    cp $INSTALLED_ROOT/bin/haskell-ci $stage/

    cd $stage
    tar czf $src/haskell-ci-$TRAVIS_TAG-$TARGET.tar.gz *
    cd $src

    rm -rf $stage
}

main
