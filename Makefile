REBAR = $(shell pwd)/rebar
REBAR_BUILD_DIR = $(shell pwd)/.rebar-build

REBAR_TEST ?= 0

V ?= 0

rebar_args_3 = -v 3
rebar_args_2 = -v 2
rebar_args_1 = -v 1
rebar_args = $(rebar_args_$(V))

rebar_verbose_0 = @echo "REBAR";
rebar_verbose = $(rebar_verbose_$(V))

rebar = $(rebar_verbose) V=$(V) REBAR_TEST=$(REBAR_TEST) $(REBAR) $(rebar_args)

.PHONY: compile xref eunit clean distclean doc check make deps

all: compile xref eunit

deps: $(REBAR)
	$(rebar) update-deps
	$(rebar) get-deps
	$(rebar) check-deps

compile: deps
	$(rebar) compile

xref: compile
	$(rebar) xref

eunit: compile
	$(rebar) skip_deps=true eunit

clean:
	$(rebar) clean

distclean: clean
	$(rebar) delete-deps

doc:
	$(rebar) skip_deps=true doc

check: compile
#	@echo "you need ./rebar build-plt before make check"
#	@./rebar build-plt
	@./rebar check-plt
	@./rebar dialyze

crosslang:
	@echo "do ERL_LIBS=../ before you make crosslang or fail"
	cd test && make crosslang

test-deps: REBAR_TEST=1
test-deps: $(REBAR)
	$(rebar) update-deps
	$(rebar) get-deps
	$(rebar) check-deps

test-compile: REBAR_TEST=1
test-compile: test-deps
	$(rebar) compile

test: REBAR_TEST=1
test: clean test-compile
	$(rebar) skip_deps=true ct

$(REBAR):
	@rm -rf $(REBAR_BUILD_DIR)
	git clone git://github.com/basho/rebar.git $(REBAR_BUILD_DIR)
	cd $(REBAR_BUILD_DIR) && ./bootstrap
	mv $(REBAR_BUILD_DIR)/rebar $(REBAR)
	@rm -rf $(REBAR_BUILD_DIR)
