TARGET := block-test-completeness
SRC_ADB := component.adb completeness.adb
ADA_COMPONENTS_DIR = $(REP_DIR)/../../components
INC_DIR := $(ADA_COMPONENTS_DIR)/common/block_completeness
LIBS := base spark ada_interface libsparkcrypto sxml
vpath %.adb $(ADA_COMPONENTS_DIR)/common/block_completeness
