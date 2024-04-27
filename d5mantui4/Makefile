release:
	-rm _checkouts/cecho/priv/cecho.so _checkouts/cecho/c_src/cecho.o
	rebar3 release

compile:
	rebar3 compile

run: release
	unset -v LINES; unset -v COLUMNS; exec ./_build/default/rel/d5mantui/bin/d5mantui foreground
