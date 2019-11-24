#!/bin/sh -e

[ ! -d pages_erlang ] || rm -rf pages_erlang
mkdir pages_erlang

MDVL_CI_PHOENIX_ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"

#------------------------------------------------[ Erlang Main Documentation ]--

"$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" \
			extract ial_in_erlang.deb pages_erlang erlang-doc

mv pages_erlang/usr/share/doc/erlang-doc pages_erlang/erlang_att
rm -r pages_erlang/usr

cat > pages_erlang/erlang.yml <<EOF
section: 21
x-masysma-name: erlang
keywords: ["erlang", "ial_toplevel"]
x-masysma-redirect: doc/index.html
---
EOF

# A bit of an inefficient implementation (quadratic), but fast enough for now.
# a)    <td><a href="../lib/sasl-3.3/doc/html/alarm_handler.html">alarm_handler</a></td>
# b)    <li title="which_applications-1"><a href="application.html#which_applications-1">which_applications/1</a></li>
abscut=$(($(cd pages_erlang/erlang_att; pwd | wc -c) + 1))
knowroot=
grep -E '^    <td><a href' < pages_erlang/erlang_att/doc/man_index.html | \
							while read -r line; do
	file="pages_erlang/erlang_att/doc/$(echo "$line" | cut -d"\"" -f 2)"
	relroot="$(cd "$(dirname "$file")"; pwd | cut -c "${abscut}-" | \
							sed 's/\//\\\//g')"
	if printf "%s" "$knowroot" | grep -qF "$relroot"; then
		continue
	else
		knowroot="$knowroot
$relroot"
	fi
	grep -E '^    <li title="[^"]+"><a href="[^"]+">[^<]+</a></li>$' < "$file" | sed 's/^    <li title="[^"]\+"><a href="\([^#]\+\)\.html#\([^#]\+\)">\([^<]\+\)<\/a><\/li>$/section: 21\nx-masysma-name: erlang\/\1:\3\nkeywords: ["erlang", "\1:\3", "\1", "\3"]\nx-masysma-redirect: '$relroot'\/\1.html#\2\n---/g' >> pages_erlang/erlang.yml
done

#--------------------------------------------------------[ Erlang Webmachine ]--

"$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" extract \
		ial_in_webmachine_wiki.git pages_erlang/erlang_webmachine_att \
		https://github.com/webmachine/webmachine.wiki.git
rm -rf "pages_erlang/erlang_webmachine_att/.git"
plist=
for i in pages_erlang/erlang_webmachine_att/*.md; do
	fnbr="$(basename "$i" | cut -d. -f 1)"
	if ! [ "$fnbr" = Home ]; then
		plist="$plist
<li><a href=\"$fnbr.html\">$fnbr</a></li>"
	fi
done
cat > pages_erlang/erlang_webmachine.yml <<EOF
section: 21
x-masysma-name: erlang/webmachine
keywords: ["erlang", "webmachine", "ial_toplevel"]
x-masysma-redirect: Home.html
---
EOF
for i in pages_erlang/erlang_webmachine_att/*.md \
				pages_erlang/erlang_webmachine_att/*.org; do
	fnbr="$(basename "$i" | cut -d. -f 1)"
	ftype=
	prefix=
	case "$(basename "$i" | cut -d. -f 2)" in
	(md) ftype=gfm;;
	(org) prefix="#+OPTIONS: ^:nil"; ftype=org+backtick_code_blocks;;
	esac
	fnrel="pages_erlang/erlang_webmachine_att/$fnbr.html"
	cat > "$fnrel" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
			"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>$fnbr</title>
<style type="text/css">
/* <![CDATA[ */
#matoc { margin-left: 0; padding-left: 0; }
#matoc li { list-style: none; margin: 3px; padding: 5px;
				border: 1px dotted #000000; float: left; }
hr { clear: both; }
table, tr { margin: 0; padding: 0; }
table, tr, td, th { border: 1px solid #808080; border-collapse: collapse; }
td, th { padding: 5px; }
/* ]]> */
</style>
</head>
<body>
<h1><a href="Home.html">Erlang Webmachine Documentation</a></h1>
<h2>$fnbr</h2>
<ul id="matoc">$plist
</ul>
<hr/>
EOF
	fnlnk="$(echo "$fnbr" | tr - _)"
	printf "section: 21\nx-masysma-name: erlang/webmachine/%s\nkeywords: [\"erlang\", \"webmachine\", \"%s\"]\nx-masysma-redirect: %s.html\n---\n" "$fnlnk" "$fnlnk" "$fnbr.html" >> pages_erlang/erlang_webmachine.yml
	{ echo "$prefix"; ./lib_erlang_webmachine.awk "$i"; } | \
						pandoc -f "$ftype" >> "$fnrel"
	cat >> "$fnrel" <<EOF
<hr/>
</body>
</html>
EOF
done

rm pages_erlang/erlang_webmachine_att/*.md \
					pages_erlang/erlang_webmachine_att/*.org

#------------------------------------------------------------[ Erlang Cowboy ]--

"$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" \
				extract ial_in_erlang_cowboy_doc.tar.xz .
mv ial_in_erlang_cowboy_doc pages_erlang/erlang_cowboy_att

cat > pages_erlang/erlang_cowboy.yml <<EOF
section: 21
x-masysma-name: erlang/cowboy/guide
keywords: ["erlang", "cowboy", "guide", "ial_toplevel"]
x-masysma-redirect: guide/index.html
---
section: 21
x-masysma-name: erlang/cowboy/manual
keywords: ["erlang", "cowboy", "manual", "ial_toplevel"]
x-masysma-redirect: manual/index.html
---
EOF

for pagdir in pages_erlang/erlang_cowboy_att/guide/*/; do
	entry="$(basename "$pagdir")"
	printf "section: 21\nx-masysma-name: erlang/cowboy/guide/%s\nkeywords: [\"erlang\", \"cowboy\", \"guide\", \"%s\"]\nx-masysma-redirect: guide/%s/index.html\n---\n" "$entry" "$entry" "$entry"
done >> pages_erlang/erlang_cowboy.yml

for file in pages_erlang/erlang_cowboy_att/manual/cowboy*.html; do
	bn="$(basename "$file")"
	nn="$(echo "$bn" | cut -d. -f 1)"
	manpage="$(grep -F "<title>" "$file" | cut -d: -f 2 | cut -d"(" -f 1)"
	printf "section: 21\nx-masysma-name: erlang/cowboy/manual/%s\nkeywords: [\"erlang\", \"cowboy\", \"manual\", \"%s\"]\nx-masysma-redirect: manual/%s\n---\n" "$nn" "$nn" "$bn"
done >> pages_erlang/erlang_cowboy.yml
