#!/usr/bin/env bash

BUNDLE=./scripts/icon.js

mkdir -p $(basename $BUNDLE)

spago bundle-app -p ./bin/codegen/Icon.purs -m Bin.Codegen.Icon --to "$BUNDLE"

node $BUNDLE $@


