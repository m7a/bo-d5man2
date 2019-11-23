#!/bin/sh -e

[ ! -d pages_posix ] || rm -r pages_posix
mkdir pages_posix

MDVL_CI_PHOENIX_ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
"$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" \
	extract ial_in_posix.tar.bz2 pages_posix \
	https://pubs.opengroup.org/onlinepubs/9699919799/download/susv4-2018.tar.bz2

mv pages_posix/* pages_posix/posix_att
cat > pages_posix/posix.yml <<EOF
section: 21
x-masysma-name: posix
keywords: ["posix", "ial_toplevel"]
x-masysma-redirect: index.html
---
EOF
cat pages_posix/posix_att/idx/i?.html | grep -F disc | \
	sed 's/<li type=disc><a href="\.\.\/\([^"]\+\)">\([^<]\+\)<\/a>.*$/section: 21\nx-masysma-name: posix\/\2\nkeywords: ["posix", "\2"]\nx-masysma-redirect: \1\n---/g' >> pages_posix/posix.yml
