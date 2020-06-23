#!/usr/bin/env bash

spago run --node-args "codegen --all-components" --config "codegen.dhall"

find ./src -name '*.purs' -exec purty --write '{}' \;
