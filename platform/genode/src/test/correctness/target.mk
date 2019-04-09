TARGET := block-test-correctness
SRC_ADB := component.adb correctness.adb ringbuffer.adb
ADA_COMPONENTS_DIR = $(REP_DIR)/../../components
INC_DIR := $(ADA_COMPONENTS_DIR)/common/block_correctness
LIBS := base spark ada_interface libsparkcrypto permutation
vpath %.adb $(ADA_COMPONENTS_DIR)/common/block_correctness
