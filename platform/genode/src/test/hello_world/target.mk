TARGET := test-hello_world
SRC_ADB := hello_world.adb
LIBS := base spark ada_interface
ADA_COMPONENTS_DIR = $(REP_DIR)/../../libs/ada-interface/test/hello_world
INC_DIR += $(ADA_COMPONENTS_DIR)
vpath hello_world.adb $(ADA_COMPONENTS_DIR)
