#!/bin/sh -e

[ ! -d pages_jargon ] || rm -rf pages_jargon
mkdir pages_jargon

MDVL_CI_PHOENIX_ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
"$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" \
				extract ial_in_jargon.tar.gz pages_jargon \
				http://catb.org/jargon/jargon-4.4.7.tar.gz

mv pages_jargon/jargon-*.*.* pages_jargon/jargon_att

cat > pages_jargon/jargon.yml <<EOF
section: 21
x-masysma-name: jargon
keywords: ["jargon", "ial_toplevel"]
x-masysma-redirect: html/index.html
EOF
sed -e 's/<a href="\([^"]\+\)">\([^<]\+\)<\/a>/\nENTRY,\1,\2\n/g' -e 's/ /_/g' \
	< pages_jargon/jargon_att/html/go01.html | \
	grep -E "^ENTRY" | \
	sed 's/^ENTRY,\([^,]\+\),\([^,]\+\)$/section: 21\nx-masysma-name: jargon\/\2\nkeywords: ["jargon", "\2"]\nx-masysma-redirect: html\/\1\n---/g' \
	>> pages_jargon/jargon.yml
