default: test

compile:
	rebar compile skip_deps=true

clean:
	rebar clean

test:
ifdef suites
	rebar eunit skip_deps=true suites=$(suites)
else
	rebar eunit skip_deps=true
endif

.PHONY: default compile clean test
