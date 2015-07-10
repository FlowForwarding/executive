.PHONY: compile get-deps update-deps clean deep-clean no-compile-rel rel test rebar

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

test:
	mkdir -p /tmp/ct_log
	ct_run -pa deps/*/ebin -pa ebin/ -dir test/ -logdir /tmp/ct_log -noshell

rebar:
	wget -c https://github.com/rebar/rebar/wiki/rebar
	chmod +x rebar
