PROJECT=ibrowse
PLT_APPS=erts kernel stdlib ssl crypto public_key
TEST_ERLC_OPTS=-pa ../ibrowse/ebin

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

test: app eunit unit_tests old_tests
	@echo "====================================================="

unit_tests:
	@echo "====================================================="
	@echo "Running tests..."
	@cd test && make test && cd ..

old_tests:
	@echo "====================================================="
	@echo "Running old tests..."
	@cd test && make old_tests && cd ..
