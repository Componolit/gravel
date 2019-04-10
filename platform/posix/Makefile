
clean:
	make -C ../../libs/ada-interface/ada-runtime clean
	rm -rf gprbuild/build gprbuild/lib

../../libs/ada-interface/ada-runtime/obj/lib/libposix_rts.a:
	make -C ../../libs/ada-interface/ada-runtime
	make -C ../../libs/ada-interface/ada-runtime platform

runtime: ../../libs/ada-interface/ada-runtime/obj/lib/libposix_rts.a

.SECONDEXPANSION:
DEPS=$$(call deps,$$@)
.SECONDEXPANSION:
LIB_DEPS=$$(call deps,gprbuild/$$(notdir $$(basename $$@)).gpr)

deps=$(addprefix gprbuild/lib/,$(shell cat $(1) | grep -E "^with \"[^\.]" | sed "s/with \"\(.*\)\.gpr.*/\1/g"))

gprbuild/lib/%: runtime $(LIB_DEPS)
	gprbuild -j4 -P gprbuild/$(notdir $@).gpr -XOPERATION=compile

gprbuild/lib/%: runtime $(LIB_DEPS)
	gprbuild -j4 -P gprbuild/$(notdir $@).gpr -XOPERATION=compile

%.gpr: runtime $(DEPS)
	gprbuild -j4 -P $@
