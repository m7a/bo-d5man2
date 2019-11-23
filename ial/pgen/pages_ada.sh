#!/bin/sh -e

[ ! -d pages_ada ] || rm -r pages_ada
mkdir pages_ada

MDVL_CI_PHOENIX_ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
"$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" \
		extract ial_in_ada.deb pages_ada ada-reference-manual-2012

mv pages_ada/usr/share/doc/ada-reference-manual-2012/arm2012.html \
							pages_ada/ada_att
rm -r pages_ada/usr

./lib_ada_proc.php > pages_ada/ada.yml
