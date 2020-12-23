#!/usr/bin/env bash

spago run --node-args "codegen --all-components" --main Codegen.Main --config "codegen.dhall"

# find ./src -name '*.purs' | while read f; do
#   sed 's/⇒/=>/g' -i "$f"
#   sed 's/→/->/g' -i "$f"
#   sed 's/←/<-/g' -i "$f"
#   sed 's/∷/::/g' -i "$f"
#   sed 's/⇐/<=/g' -i "$f"
# done

find ./src -name '*.purs' -exec purty --write '{}' \;
