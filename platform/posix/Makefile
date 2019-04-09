
clean:
	make -C ../../libs/ada-interface/ada-runtime clean
	rm -rf gprbuild/build gprbuild/lib

../../libs/ada-interface/ada-runtime/obj/lib/libposix_rts.a:
	make -C ../../libs/ada-interface/ada-runtime
	make -C ../../libs/ada-interface/ada-runtime platform

runtime: ../../libs/ada-interface/ada-runtime/obj/lib/libposix_rts.a

gprbuild/lib/permutation: runtime
	gprbuild -j4 -P gprbuild/permutation.gpr -XOPERATION=compile

gprbuild/lib/sparkcrypto: runtime
	gprbuild -j4 -P gprbuild/libsparkcrypto.gpr -XOPERATION=compile

libsparkcrypto: gprbuild/lib/sparkcrypto

permutation: gprbuild/lib/permutation

libs: libsparkcrypto permutation

component=$(shell echo $(1) | sed -e "s/\(.*\)\..*/\1/g")

%.component: runtime libs
	gprbuild -j4 -P gprbuild/component.gpr -XNAME=$(call component,$@) -XPATH=$(shell find ../../ -name $(call component,$@) | head -n1)
