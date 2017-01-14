all:
	rebar get-deps compile escriptize

clean:
	rebar clean
	rm -rf deps ebin

