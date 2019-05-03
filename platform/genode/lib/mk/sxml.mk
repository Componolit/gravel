
include $(REP_DIR)/lib/import/import-sxml.mk

SRC_ADB += sxml.adb \
	   sxml-generator.adb \
	   sxml-parser.adb \
	   sxml-query.adb \
	   sxml-serialize.adb
SRC_CC += mem.c
LIBS = base spark

vpath %.adb $(LIB_DIR)
vpath %.cc  $(REP_DIR)/src/lib/sxml
