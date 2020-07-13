#!/usr/bin/env bash

spago run --node-args "codegen --all-components" --main Codegen.Main --config "codegen.dhall"

find ./src -name '*.purs' -exec purty --write '{}' \;
