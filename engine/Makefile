all: app

app: get-deps
	@./rebar compile

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean
	@rm -f erl_crash.dump

dist-clean: clean
