GENODE_TESTS ?= \
    gravel/parpen_no_mem_session \
    gravel/parpen_invalid_operation \
    gravel/parpen_name_service \

#    gravel/parpen

help:
	@echo "help:"
	@echo "   make generate: generate sources from RecordFlux specification"
	@echo "   make test: run test cases"

test: test_aunit test_genode

test_aunit:
	@$(MAKE) -C test

test_genode:
	@$(MAKE) --silent -C ../../../../../../../build/x86_64 $(addprefix run/,$(GENODE_TESTS))

generate:
	@rm -f generated/*
	@rflx generate -d generated -p Parpen -r specs/binder.rflx specs/protocol.rflx specs/service_manager.rflx
	@git checkout generated/parpen-generic_types.adb
