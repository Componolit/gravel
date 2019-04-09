TARGET := block-test-latency
SRC_ADB := component.adb iteration.adb run.adb rwr.adb
ADA_COMPONENTS_DIR = $(REP_DIR)/../../components
INC_DIR := $(ADA_COMPONENTS_DIR)/common/block_latency
LIBS := base spark ada_interface
vpath %.adb $(ADA_COMPONENTS_DIR)/common/block_latency
