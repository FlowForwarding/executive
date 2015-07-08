.PHONY: compile get-deps rebar

compile: get-deps
	./rebar compile

get-deps:
	./rebar get-deps

clean:
	./rebar clean

deep-clean: clean
	rm -rf deps/*/ebin/*

rebar:
	wget -c https://github.com/rebar/rebar/wiki/rebar
	chmod +x rebar
