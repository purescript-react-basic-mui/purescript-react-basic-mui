#!/usr/bin/env bash

# Compiled script goes into library codegen subdir
CODEGEN_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
BUNDLE="$CODEGEN_DIR/IconBundle.js"

mkdir -p $(basename $BUNDLE)

if [ ! -f $BUNDLE ]; then
  spago bundle-app -p "$CODEGEN_DIR/Icon.purs" -m Bin.Codegen.Icon --to "$BUNDLE"
fi

node $BUNDLE $@

