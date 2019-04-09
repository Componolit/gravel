TARGET := test-block_client
SRC_ADB := ada_block_test.adb
LIBS := base spark ada_interface
ADA_COMPONENTS_DIR = $(REP_DIR)/../../libs/ada-interface/test/block_client
INC_DIR := $(ADA_COMPONENTS_DIR)
vpath ada_block_test.adb $(ADA_COMPONENTS_DIR)
