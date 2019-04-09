
CAI_INC_DIR = $(REP_DIR)/../../libs/ada-interface/src
CAI_PLATFORM_DIR = $(REP_DIR)/../../libs/ada-interface/platform/genode

INC_DIR += $(CAI_INC_DIR) $(CAI_PLATFORM_DIR)

SRC_CC += cai_main.cc
SRC_ADB += cai-component.adb

vpath cai_main.cc $(CAI_PLATFORM_DIR)
vpath cai-component.adb $(CAI_PLATFORM_DIR)
