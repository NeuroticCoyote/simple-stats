shell:
	rebar3 shell --sname=simple_stats --setcookie cookie

clean:
	rebar3 clean
	rm rebar.lock
	rm -rf log
	rm -rf _build