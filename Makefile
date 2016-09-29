PROJECT = ranch_ha
PROJECT_VERSION = 0.1

DEPS = ranch

-include erlang.mk

TEST_SPECS = test/ranch_ha.spec

app:: $(TEST_SPECS)

clean:: clean-local

%.spec: %.spec.in
	$(gen_verbose)
	@echo "% Generated on $(shell date)"> $@
	@cat $< | sed \
	  -e 's,@TOPDIR@,'$(CURDIR)',' \
	  -e 's,@TEST_DIR@,'$(TEST_DIR)',' \
	  -e 's,@PATHS@,'$(CURDIR)/ebin' '$(DEPS_DIR)/*/ebin' '$(APPS_DIR)/*/ebin' '$(TEST_DIR)',' \
	  > $@

clean-local:
	$(gen_verbose) rm -f $(TEST_SPECS)

.PHONY: clean-local
