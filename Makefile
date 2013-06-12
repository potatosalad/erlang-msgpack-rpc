REBAR = $(shell pwd)/rebar
REBAR_BUILD_DIR = $(shell pwd)/.rebar-build
TREBAR = $(REBAR) -C rebar.test.config

.PHONY: compile xref eunit clean distclean doc check make deps

all: compile xref eunit

deps: $(REBAR)
	@$(REBAR) update-deps
	@$(REBAR) get-deps
	@$(REBAR) check-deps

compile: deps
	@$(REBAR) compile

xref: compile
	@$(REBAR) xref

eunit: compile
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

doc:
	@$(REBAR) skip_deps=true doc

check: compile
#	@echo "you need ./rebar build-plt before make check"
#	@./rebar build-plt
	@./rebar check-plt
	@./rebar dialyze

crosslang:
	@echo "do ERL_LIBS=../ before you make crosslang or fail"
	cd test && make crosslang

test-deps: $(REBAR)
	@$(TREBAR) update-deps
	@$(TREBAR) get-deps
	@$(TREBAR) check-deps

test-compile: test-deps
	@$(TREBAR) compile

test: clean test-compile
	@$(TREBAR) skip_deps=true ct

$(REBAR):
	@rm -rf $(REBAR_BUILD_DIR)
	git clone git://github.com/basho/rebar.git $(REBAR_BUILD_DIR)
	cd $(REBAR_BUILD_DIR) && ./bootstrap
	mv $(REBAR_BUILD_DIR)/rebar $(REBAR)
	@rm -rf $(REBAR_BUILD_DIR)
