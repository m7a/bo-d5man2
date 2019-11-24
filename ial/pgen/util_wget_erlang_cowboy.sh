#!/bin/sh -e
# Ma_Sys.ma wget commandline 1.1.0, Copyright (c) 2013, 2019 Ma_Sys.ma.
# For further info sen an e-mail to Ma_Sys.ma@web.de.

# Design notice:
# As there does not seem to be a way to quickly obtain the manpage-style
# pages in HTML format, here is a wget-based approach. It is somehow strange
# that all pages are already in a text format and get downloaded as HTML instead
# of directly processing the text format. Currently, this happens for reasons
# of uniformity (all IAL pages are HTML or PDF and the "pages"-style items
# are all HTML). New D5Man will not support live-processing of pages in the
# way that old `d5manserver` did, thus having the text pages directly here
# disables them in the IAL webinterface (might still be an option to consider)

[ ! -d dl_erlang_cowboy ] || rm -r dl_erlang_cowboy
mkdir dl_erlang_cowboy
cd dl_erlang_cowboy

dir=https://ninenines.eu/docs/en/cowboy
sub=2.7/guide/

wget "--base=$dir" --timeout=300 --wait=1 --waitretry=5 \
	--restrict-file-names=windows --adjust-extension \
	--recursive --convert-links --page-requisites \
	--relative --level=1 "$dir/$sub"

dir=https://ninenines.eu/docs/en/cowboy
sub=2.7/manual/

wget "--base=$dir" --timeout=300 --wait=1 --waitretry=5 \
	--restrict-file-names=windows --adjust-extension \
	--recursive --convert-links --page-requisites \
	--relative --level=1 "$dir/$sub"

cd ..
mv dl_erlang_cowboy/ninenines.eu/docs/en/cowboy/2.7 ial_in_erlang_cowboy_doc
rm -r dl_erlang_cowboy
tar -c ial_in_erlang_cowboy_doc | xz -9 > ial_in_erlang_cowboy_doc.tar.xz
