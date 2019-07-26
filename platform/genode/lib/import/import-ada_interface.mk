
CAI_INC_DIR = $(REP_DIR)/../../libs/ada-interface/src
CAI_BLOCK_INC_DIR = $(CAI_INC_DIR)/block
CAI_LOG_INC_DIR = $(CAI_INC_DIR)/log
CAI_TIMER_INC_DIR = $(CAI_INC_DIR)/timer
CAI_PLATFORM_DIR = $(CAI_INC_DIR)/platform/genode
CAI_CONFIG_DIR = $(CAI_INC_DIR)/rom
CAI_COMMON_DIR = $(CAI_INC_DIR)/common

INC_DIR += $(CAI_INC_DIR) \
	   $(CAI_COMMON_DIR) \
	   $(CAI_PLATFORM_DIR) \
	   $(CAI_BLOCK_INC_DIR) \
	   $(CAI_BLOCK_INC_DIR)/genode \
	   $(CAI_BLOCK_INC_DIR)/client \
	   $(CAI_BLOCK_INC_DIR)/client/genode \
	   $(CAI_BLOCK_INC_DIR)/server \
	   $(CAI_BLOCK_INC_DIR)/server/genode \
	   $(CAI_LOG_INC_DIR) \
	   $(CAI_LOG_INC_DIR)/genode \
	   $(CAI_LOG_INC_DIR)/client \
	   $(CAI_LOG_INC_DIR)/client/genode \
	   $(CAI_TIMER_INC_DIR) \
	   $(CAI_TIMER_INC_DIR)/genode \
	   $(CAI_TIMER_INC_DIR)/client \
	   $(CAI_TIMER_INC_DIR)/client/genode \
	   $(CAI_CONFIG_DIR) \
	   $(CAI_CONFIG_DIR)/genode \
	   $(CAI_CONFIG_DIR)/client \
	   $(CAI_CONFIG_DIR)/client/genode

SRC_CC += cai_main.cc
SRC_ADB += componolit-interfaces-component.adb

vpath cai_main.cc $(CAI_PLATFORM_DIR)
vpath componolit-interfaces-component.adb $(CAI_PLATFORM_DIR)
