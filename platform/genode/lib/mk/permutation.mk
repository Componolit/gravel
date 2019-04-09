
include $(REP_DIR)/lib/import/import-permutation.mk

SRC_ADB += permutation.adb
LIBS = base spark

vpath permutation.adb $(LIB_DIR)
