PROJECT = ranch_ha
PROJECT_VERSION = 0.1

DEPS = ranch

-include erlang.mk

TEST_HOSTNAME = $(shell hostname -s)"."$(shell hostname -d)
TEST_PATHS = $(CURDIR)/ebin" "$(DEPS_DIR)/*/ebin" "$(APPS_DIR)/*/ebin" "$(TEST_DIR)
TEST_SPECS = test/single.spec test/cluster.spec
TEST_TARGETS = $(patsubst test/%.spec,test/%.test,$(TEST_SPECS))

app:: $(TEST_SPECS)

clean:: clean-local

dtests: $(TEST_TARGETS)

%.test: %.spec
	erl -name ct@$(TEST_HOSTNAME) -pa $(TEST_PATHS) -eval "ct_master:run(\"$<\"), erlang:halt(0)."
	sleep 3

%.spec: %.spec.in
	$(gen_verbose)
	@echo "% Generated on $(shell date)"> $@
	@cat $< | sed \
	  -e 's,@TOPDIR@,'$(CURDIR)',' \
	  -e 's,@TEST_DIR@,'$(TEST_DIR)',' \
	  -e 's,@PATHS@,'$(TEST_PATHS)',' \
	  -e 's,@HOSTNAME@,'$(TEST_HOSTNAME)',' \
	  > $@

clean-local:
	$(gen_verbose) rm -f $(TEST_SPECS)

.PHONY: dtests clean-local
