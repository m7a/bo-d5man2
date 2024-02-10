release:
	-rm /data/main/114_projects_wrk_wrpru/114.164_cecho/priv/cecho.so
	rebar3 release

compile:
	rebar3 compile

run: release
	./_build/default/rel/d5mantui/bin/d5mantui foreground

#run: compile
#	rebar3 shell

