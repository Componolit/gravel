
include $(REP_DIR)/lib/import/import-ada_interface.mk

CC_CXX_OPT += -Wno-attributes

SRC_ADS += cxx.ads \
	   cxx-block.ads \
	   cxx-block-client.ads \
	   cxx-genode.ads \
	   cxx-log.ads \
	   cxx-log-client.ads \
	   componolit-interfaces-internal.ads \
	   componolit-interfaces-internal-block.ads \
	   componolit-interfaces-internal-log.ads \
	   componolit-interfaces.ads \
	   componolit-interfaces-types.ads \
	   componolit-interfaces-internal-types.ads \
	   componolit-interfaces-block.ads \
	   componolit-interfaces-timer.ads \
	   componolit-interfaces-internal-timer.ads \
	   cxx-timer.ads \
	   cxx-timer-client.ads \
	   componolit-interfaces-rom.ads \
	   cxx-configuration.ads \
	   cxx-configuration-client.ads \
	   componolit-interfaces-internal-rom.ads

SRC_ADB += cxx-block-server.adb \
	   cxx-block-dispatcher.adb \
	   componolit-interfaces-block-client.adb \
	   componolit-interfaces-block-server.adb \
	   componolit-interfaces-block-dispatcher.adb \
	   componolit-interfaces-block-util.adb \
	   componolit-interfaces-log.adb \
	   componolit-interfaces-log-client.adb \
	   componolit-interfaces-timer-client.adb \
	   componolit-interfaces-rom-client.adb

SRC_CC += cai_factory.cc \
	  block_client.cc \
	  block_dispatcher.cc \
	  block_server.cc \
	  log_client.cc \
	  timer_client.cc \
	  configuration_client.cc

LIBS = base spark

vpath cxx.ads $(CAI_PLATFORM_DIR)
vpath cxx-block.ads $(CAI_BLOCK_INC_DIR)/genode
vpath cxx-block-client.ads $(CAI_BLOCK_INC_DIR)/client/genode
vpath cxx-genode.ads $(CAI_PLATFORM_DIR)
vpath cxx-log.ads $(CAI_LOG_INC_DIR)/genode
vpath cxx-log-client.ads $(CAI_LOG_INC_DIR)/client/genode
vpath componolit-interfaces-internal.ads $(CAI_PLATFORM_DIR)
vpath componolit-interfaces-internal-block.ads $(CAI_BLOCK_INC_DIR)/genode
vpath componolit-interfaces-internal-log.ads $(CAI_LOG_INC_DIR)/genode
vpath componolit-interfaces.ads $(CAI_INC_DIR)
vpath componolit-interfaces-types.ads $(CAI_INC_DIR)
vpath componolit-interfaces-internal-types.ads $(CAI_PLATFORM_DIR)
vpath componolit-interfaces-block.ads $(CAI_BLOCK_INC_DIR)
vpath componolit-interfaces-timer.ads $(CAI_TIMER_INC_DIR)
vpath componolit-interfaces-internal-timer.ads $(CAI_TIMER_INC_DIR)/genode
vpath cxx-timer.ads $(CAI_TIMER_INC_DIR)/genode
vpath cxx-timer-client.ads $(CAI_TIMER_INC_DIR)/client/genode
vpath componolit-interfaces-rom.ads $(CAI_CONFIG_DIR)
vpath cxx-configuration.ads $(CAI_CONFIG_DIR)/client/genode
vpath cxx-configuration-client.ads $(CAI_CONFIG_DIR)/client/genode
vpath componolit-interfaces-internal-rom.ads $(CAI_CONFIG_DIR)/genode
vpath cxx-block-server.adb $(CAI_BLOCK_INC_DIR)/server/genode
vpath cxx-block-dispatcher.adb $(CAI_BLOCK_INC_DIR)/server/genode
vpath componolit-interfaces-block-client.adb $(CAI_BLOCK_INC_DIR)/client/genode
vpath componolit-interfaces-block-server.adb $(CAI_BLOCK_INC_DIR)/server/genode
vpath componolit-interfaces-block-dispatcher.adb $(CAI_BLOCK_INC_DIR)/server/genode
vpath componolit-interfaces-block-util.adb $(CAI_BLOCK_INC_DIR)/genode
vpath componolit-interfaces-log.adb $(CAI_LOG_INC_DIR)
vpath componolit-interfaces-log-client.adb $(CAI_LOG_INC_DIR)/client/genode
vpath componolit-interfaces-timer-client.adb $(CAI_TIMER_INC_DIR)/client/genode
vpath componolit-interfaces-rom-client.adb $(CAI_CONFIG_DIR)/client/genode
vpath cai_factory.cc $(CAI_PLATFORM_DIR)
vpath block_client.cc $(CAI_BLOCK_INC_DIR)/client/genode
vpath block_dispatcher.cc $(CAI_BLOCK_INC_DIR)/server/genode
vpath block_server.cc $(CAI_BLOCK_INC_DIR)/server/genode
vpath log_client.cc $(CAI_LOG_INC_DIR)/client/genode
vpath timer_client.cc $(CAI_TIMER_INC_DIR)/client/genode
vpath configuration_client.cc $(CAI_CONFIG_DIR)/client/genode

SHARED_LIB = yes
