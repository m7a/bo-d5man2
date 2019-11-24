#!/bin/sh -e

# Inputs:
#
# 	ial_in_java_11.deb  (downloadable)
# 	ial_in_java_xz.deb  (downloadable)
# 	ial_in_java_8.deb   (not downloadable / archive.debian.org)
# 	ial_in_java_fx8.deb (not downloadable / archive.debian.org)
# 	ial_in_java_7.deb   (not downloadable / archive.debian.org)
# 	ial_in_lejos.tar.xz (not reproducible)
# 	$MDVL_CI_PHOENIX_ROOT/bp-jexer (required)
#
# Non-downloadable artifacts will be skipped if missing except for bp-jexer
# repository which can be cloned from https://github.com/m7a/bp-jexer

#------------------------------------------------------------------[ General ]--

MDVL_CI_PHOENIX_ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"

# $1: api_prefix for redirect
# $2: name (also for ial_toplevel)
# $3+ add tags
classes_to_yml() {
	api_prefix="$1"
	name="$2"
	shift 2
	addtags="$*"
	cat <<EOF
section: 21
x-masysma-name: java/$name
keywords: ["java", $addtags"ial_toplevel"]
x-masysma-redirect: ${api_prefix}index.html
---
EOF
	grep -E '^<li>.*title=".*$' | \
		sed -e 's/<span class="[^"]\+">\([^<]\+\)<\/span>/\1/g' -e 's/<li><a href="\([^"]\+\)" title="[a-z0-9]\+ in \([^"]\+\)"[^>]*>\(<i>\)\?\([^<]\+\)\(<\/i>\)\?<\/a><\/li>$/section: 21\nx-masysma-name: java\/'"$name"'\/\2.\4\nkeywords: ["java", '"$addtags"'"\2", "\4", "\2.\4"]\nx-masysma-redirect: '"$api_prefix"'\1\n---/g'
}

[ ! -d pages_java ] || rm -r pages_java
mkdir pages_java

#----------------------------------------------------------------[ Downloads ]--

"$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" require \
					ial_in_java_11.deb openjdk-11-doc
"$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" require \
					ial_in_java_xz.deb libxz-java-doc

#----------------------------------------------------------------[ Jexer API ]--

( cd "$MDVL_CI_PHOENIX_ROOT/bp-jexer" && ant doc; )
mv "$MDVL_CI_PHOENIX_ROOT/bp-jexer/build/docs/api" pages_java/java_jexer_att
classes_to_yml "" "jexer" "\"jexer\", " \
			< "pages_java/java_jexer_att/allclasses-noframe.html" \
			> pages_java/java_jexer.yml
( cd "$MDVL_CI_PHOENIX_ROOT/bp-jexer" && ant dist-clean; )

#----------------------------------------------------------------[ Lejos NXJ ]--

isfail=0
if "$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" extract \
					ial_in_lejos.tar.xz pages_java; then
	mv pages_java/lejos pages_java/java_lejos_att
	cat > pages_java/java_lejos.yml <<EOF
section: 21
x-masysma-name: java/lejos
keywords: ["java", "lejos", "ial_toplevel"]
x-masysma-redirect: index.html
---
EOF
	grep -E '^<A.*title=".*$' pages_java/java_lejos_att/allclasses-noframe.html | sed 's/^<A HREF="\([^"]\+\)" title="[a-z0-9]\+ in \([^"]\+\)"[^>]*>\(<I>\)\?\([^<]\+\)\(<\/I>\)\?<\/A>\s*$/section: 21\nx-masysma-name: java\/lejos\/\2.\4\nkeywords: ["java", "lejos", "\2", "\4", "\2.\4"]\nx-masysma-redirect: \1\n---/g' >> pages_java/java_lejos.yml
else
	echo "[WARNING] Skipped LejosNXJ (non-downloadable resource)."
	isfail=1
fi

#-----------------------------------------------------------[ JavaSE, JavaFX ]--

for i in 7 8 fx8 11 xz; do
	api="api/"
	tags="\"java$i\", "
	if ! "$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" extract \
					ial_in_java_$i.deb pages_java; then
		echo "[WARNING] Skipped java$i (non-downloadable resource)."
		isfail=1
	fi
	case "$i" in
	(fx8)
		api=
		mv pages_java/usr/share/doc/libopenjfx-java/api \
						"pages_java/java_${i}_att";;
	(xz)
		api=
		tags="\"xz\", \"tukaani\", "
		mv pages_java/usr/share/doc/libxz-java/api \
						"pages_java/java_${i}_att";;
	(*)
		mv "pages_java/usr/share/doc/openjdk-$i-jre-headless" \
						"pages_java/java_${i}_att";;
	esac

	rm -r pages_java/usr
	classes="pages_java/java_${i}_att/${api}allclasses.html"
	[ -f "$classes" ] || \
		classes="pages_java/java_${i}_att/${api}allclasses-frame.html"

	api="$(echo "$api" | sed 's/\//\\\//g')"
	classes_to_yml "$api" "$i" "$tags" < "$classes" \
						> "pages_java/java_$i.yml"
done

# only returns 0 if everythng was available...
exit $isfail
