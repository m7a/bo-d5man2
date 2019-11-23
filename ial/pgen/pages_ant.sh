#!/bin/sh -e

# Does not currently include the API documentation...

[ ! -d pages_ant ] || rm -r pages_ant
mkdir pages_ant

MDVL_CI_PHOENIX_ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
"$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" \
					extract ial_in_ant.deb pages_ant ant-doc

mv pages_ant/usr/share/doc/ant/manual pages_ant/ant_att
rm -r pages_ant/usr

cat > pages_ant/ant.yml <<EOF
section: 21
x-masysma-name: ant
keywords: ["ant", "ial_toplevel"]
x-masysma-redirect: index.html
---
EOF
grep -E '^  <li><a href="Tasks/' pages_ant/ant_att/tasklist.html | \
	sed -e 's/<em>\([^<]\+\)<\/em>/\1/g' -e 's/^  <li><a href="\([^"]\+\)">\([^<]\+\)<\/a>.*<\/li>$/section: 21\nx-masysma-name: ant\/\2\nkeywords: ["ant", "\2"]\nx-masysma-redirect: \1\n---/g' \
	>> pages_ant/ant.yml
