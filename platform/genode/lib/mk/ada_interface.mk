
include $(REP_DIR)/lib/import/import-ada_interface.mk

CC_CXX_OPT += -Wno-attributes

SRC_ADS += cxx.ads \
	   cxx-block.ads \
	   cxx-block-client.ads \
	   cxx-block-dispatcher.ads \
	   cxx-genode.ads \
	   cxx-log.ads \
	   cxx-log-client.ads \
	   cai-internal.ads \
	   cai-internal-block.ads \
	   cai-internal-log.ads \
	   cai.ads \
	   cai-types.ads \
	   cai-internal-types.ads \
	   cai-block.ads

SRC_ADB += cxx-block-server.adb \
	   cai-block-client.adb \
	   cai-block-server.adb \
	   cai-block-dispatcher.adb \
	   cai-block-util.adb \
	   cai-log.adb \
	   cai-log-client.adb

SRC_CC += block_client.cc \
	  block_dispatcher.cc \
	  block_server.cc \
	  log_client.cc

LIBS = base spark

vpath %.cc $(CAI_PLATFORM_DIR)
vpath %.ads $(CAI_PLATFORM_DIR) $(CAI_INC_DIR)
vpath %.adb $(CAI_PLATFORM_DIR) $(CAI_INC_DIR)

SHARED_LIB = yes
