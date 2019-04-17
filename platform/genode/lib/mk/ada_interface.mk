
include $(REP_DIR)/lib/import/import-ada_interface.mk

CC_CXX_OPT += -Wno-attributes

SRC_ADS += cxx.ads \
	   cxx-block.ads \
	   cxx-block-client.ads \
	   cxx-genode.ads \
	   cxx-log.ads \
	   cxx-log-client.ads \
	   cai-internal.ads \
	   cai-internal-block.ads \
	   cai-internal-log.ads \
	   cai.ads \
	   cai-types.ads \
	   cai-internal-types.ads \
	   cai-block.ads \
	   cai-timer.ads \
	   cai-internal-timer.ads \
	   cxx-timer.ads \
	   cxx-timer-client.ads

SRC_ADB += cxx-block-server.adb \
	   cxx-block-dispatcher.adb \
	   cai-block-client.adb \
	   cai-block-server.adb \
	   cai-block-dispatcher.adb \
	   cai-block-util.adb \
	   cai-log.adb \
	   cai-log-client.adb \
	   cai-timer-client.adb

SRC_CC += block_client.cc \
	  block_dispatcher.cc \
	  block_server.cc \
	  log_client.cc \
	  timer_client.cc

LIBS = base spark

INC_DIR += $(CAI_INC_DIR)/platform/genode \
	   $(CAI_LOG_INC_DIR)/genode \
	   $(CAI_LOG_INC_DIR)/client/genode \
	   $(CAI_BLOCK_INC_DIR)/genode \
	   $(CAI_BLOCK_INC_DIR)/client/genode \
	   $(CAI_BLOCK_INC_DIR)/server/genode

vpath cxx.ads $(CAI_PLATFORM_DIR)
vpath cxx-block.ads $(CAI_BLOCK_INC_DIR)/genode
vpath cxx-block-client.ads $(CAI_BLOCK_INC_DIR)/client/genode
vpath cxx-genode.ads $(CAI_PLATFORM_DIR)
vpath cxx-log.ads $(CAI_LOG_INC_DIR)/genode
vpath cxx-log-client.ads $(CAI_LOG_INC_DIR)/client/genode
vpath cai-internal.ads $(CAI_PLATFORM_DIR)
vpath cai-internal-block.ads $(CAI_BLOCK_INC_DIR)/genode
vpath cai-internal-log.ads $(CAI_LOG_INC_DIR)/genode
vpath cai.ads $(CAI_INC_DIR)
vpath cai-types.ads $(CAI_INC_DIR)
vpath cai-internal-types.ads $(CAI_PLATFORM_DIR)
vpath cai-block.ads $(CAI_BLOCK_INC_DIR)
vpath cai-timer.ads $(CAI_TIMER_INC_DIR)
vpath cai-internal-timer.ads $(CAI_TIMER_INC_DIR)/genode
vpath cxx-timer.ads $(CAI_TIMER_INC_DIR)/genode
vpath cxx-timer-client.ads $(CAI_TIMER_INC_DIR)/client/genode
vpath cxx-block-server.adb $(CAI_BLOCK_INC_DIR)/server/genode
vpath cxx-block-dispatcher.adb $(CAI_BLOCK_INC_DIR)/server/genode
vpath cai-block-client.adb $(CAI_BLOCK_INC_DIR)/client/genode
vpath cai-block-server.adb $(CAI_BLOCK_INC_DIR)/server/genode
vpath cai-block-dispatcher.adb $(CAI_BLOCK_INC_DIR)/server/genode
vpath cai-block-util.adb $(CAI_BLOCK_INC_DIR)/genode
vpath cai-log.adb $(CAI_LOG_INC_DIR)
vpath cai-log-client.adb $(CAI_LOG_INC_DIR)/client/genode
vpath cai-timer-client.adb $(CAI_TIMER_INC_DIR)/client/genode
vpath block_client.cc $(CAI_BLOCK_INC_DIR)/client/genode
vpath block_dispatcher.cc $(CAI_BLOCK_INC_DIR)/server/genode
vpath block_server.cc $(CAI_BLOCK_INC_DIR)/server/genode
vpath log_client.cc $(CAI_LOG_INC_DIR)/client/genode
vpath timer_client.cc $(CAI_TIMER_INC_DIR)/client/genode

SHARED_LIB = yes
