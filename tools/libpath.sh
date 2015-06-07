#!/bin/sh

if [ -e .cabal-sandbox ]; then
    HLIBSASS_DESC=`cabal sandbox hc-pkg describe hlibsass`
else
    HLIBSASS_DESC=`ghc-pkg describe hlibsass`
fi
echo "$HLIBSASS_DESC" | awk -F ': ' '{ if ($1 == "import-dirs") { print $2 } }'
