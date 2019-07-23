TARGET := block_dropper
SRC_ADS := block.ads
SRC_ADB := component.adb block-server.adb block-service.adb
ADA_COMPONENTS_DIR = $(REP_DIR)/../../components
INC_DIR := $(ADA_COMPONENTS_DIR)/common/block_dropper
LIBS := base spark ada_interface sxml libsparkcrypto
vpath %.ads $(ADA_COMPONENTS_DIR)/common/block_dropper
vpath %.adb $(ADA_COMPONENTS_DIR)/common/block_dropper
