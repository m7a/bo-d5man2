#!/bin/sh -e

[ ! -d pages_php ] || rm -r pages_php
mkdir pages_php

MDVL_CI_PHOENIX_ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
"$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" \
		extract ial_in_php.tar.gz pages_php \
		https://www.php.net/distributions/manual/php_manual_en.tar.gz

mv pages_php/php-chunked-xhtml pages_php/php_att

cat > pages_php/php.yml <<EOF
section: 21
x-masysma-name: php
keywords: ["php", "ial_toplevel"]
x-masysma-redirect: index.html
---
EOF

grep -E '^<li><a href.*class="index".*$' pages_php/php_att/indexes.functions.html | sed 's/<li><a href="\([^"]\+\)" class="index">\([^<]\+\)<\/a>[^<]\+\(<\/li>\)\?$/section: 21\nx-masysma-name: php\/\2\nkeywords: ["php", "\2"]\nx-masysma-redirect: \1\n---/g' | awk '/^keywords:/ { gsub(/\\/, "\\\\") } { print }' >> pages_php/php.yml
