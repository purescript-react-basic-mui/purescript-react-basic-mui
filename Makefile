bin/genIcons: scripts/GenIcons.purs scripts/GenIcons.js
	spago bundle-app -p "scripts/GenIcons.purs" -m Scripts.GenIcons --to "$@"
	sed  -i '1s;^;#!/usr/bin/env node\n;' "$@"
	chmod +x "$@"
