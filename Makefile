.PHONY: compile get-deps update-deps clean deep-clean rel no-compile-rel rebar

compile: get-deps
	./rebar compile

get-deps:
	./rebar get-deps

update-deps:
	./rebar update-deps

clean:
	./rebar clean

deep-clean: clean
	rm -rf deps/*/ebin/*

no-compile-rel:
	./relx -c _rel/relx.config

rel: compile no-compile-rel

rebar:
	wget -c https://github.com/rebar/rebar/wiki/rebar
	chmod +x rebar
