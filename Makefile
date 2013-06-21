REBAR := $(shell which rebar 2>&1 >/dev/null; if [ $$? -eq 0 ]; then which rebar; else echo $(shell pwd)/rebar; fi)
REBAR_BUILD_DIR := $(shell pwd)/.rebar-build

REBAR_TEST ?= 0

V ?= 0

rebar_args_3 = -v 3
rebar_args_2 = -v 2
rebar_args_1 = -v 1
rebar_args = $(rebar_args_$(V))

rebar_verbose_0 = @echo ":: REBAR" $(@F);
rebar_verbose = $(rebar_verbose_$(V))

rebar = $(rebar_verbose) V=$(V) REBAR_TEST=$(REBAR_TEST) $(REBAR) $(rebar_args)

DIALYZER ?= dialyzer

dialyzer_verbose_0 = @echo ":: DIALYZER" $(@F);
dialyzer_verbose = $(dialyzer_verbose_$(V))

dialyzer = $(dialyzer_verbose) $(DIALYZER)

.PHONY: deps compile clean distclean docs build-plt check-plt dialyze \
	ct eunit test-deps test-compile test xref

all: test clean compile xref

deps: $(REBAR)
	$(rebar) update-deps
	$(rebar) get-deps
	$(rebar) check-deps

compile: deps
	$(rebar) compile

clean:
	$(rebar) clean

distclean: clean
	$(rebar) delete-deps

###
### Docs
###
docs: $(REBAR)
	$(rebar) skip_deps=true doc

###
### Dialyzer
###
PLT ?= .msgpack_rpc.plt
PLT_DEPS ?= asn1 compiler crypto edoc erts gs hipe inets kernel \
	observer public_key runtime_tools sasl ssl stdlib syntax_tools \
	tools webtool xmerl
PLT_APPS ?= apps/msgpack_rpc apps/msgpack_rpc_client \
	apps/msgpack_rpc_server
DIALYZER_OPTS ?= -Werror_handling -Wno_return -Wrace_conditions \
	-Wunmatched_returns

build-plt: clean compile
	$(dialyzer) --build_plt --output_plt $(PLT) --apps $(PLT_DEPS) $(PLT_APPS) \
		deps/*/ebin apps/*/ebin

check-plt: $(PLT)
	$(dialyzer) --check_plt --plt $(PLT) --apps $(PLT_DEPS) $(PLT_APPS) \
		deps/*/ebin apps/*/ebin

dialyze: $(PLT)
	$(dialyzer) $(DIALYZER_OPTS) --plt $(PLT) deps/*/ebin apps/*/ebin

$(PLT):
	$(MAKE) build-plt

##
## Tests
##
ct: REBAR_TEST=1
ct: clean test-compile
	$(rebar) skip_deps=true ct

eunit: compile
	$(rebar) skip_deps=true eunit

test-deps: REBAR_TEST=1
test-deps: $(REBAR)
	$(rebar) update-deps
	$(rebar) get-deps
	$(rebar) check-deps

test-compile: REBAR_TEST=1
test-compile: test-deps
	$(rebar) compile

test: REBAR_TEST=1
test: clean test-compile ct

xref: compile
	$(rebar) xref

##
## rebar
##
$(REBAR):
	@rm -rf $(REBAR_BUILD_DIR)
	git clone git://github.com/basho/rebar.git $(REBAR_BUILD_DIR)
	cd $(REBAR_BUILD_DIR) && ./bootstrap
	mv $(REBAR_BUILD_DIR)/rebar $(REBAR)
	@rm -rf $(REBAR_BUILD_DIR)
