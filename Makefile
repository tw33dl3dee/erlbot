#

.PHONY: deps all clean distclean 

all:
	@(./rebar compile)

bootstrap: deps all

deps:
	@(./rebar get-deps)

clean:
	@(./rebar clean)

distclean: clean
	@(./rebar delete-deps)
