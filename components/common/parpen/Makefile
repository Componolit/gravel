help:
	@echo "help:"
	@echo "   make generate: generate sources from RecordFlux specification"

generate:
	@rm -f generated/*
	@rflx generate -d generated -p Parpen -r specs/binder.rflx specs/protocol.rflx
