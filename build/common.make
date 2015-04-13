# common parts of makefiles

all : smm.exe test_all_harness.diff smm.html

tests : test_all_harness.diff

include $(GDS_ROOT)/main_lynx_5/makerules/common_rules.make
include $(GDS_ROOT)/main_lynx_5/makerules/gprbuild_rules.make
include $(GDS_ROOT)/main_lynx_5/makerules/texinfo_rules.make

# smm.adb is not the main program, so we need this rule
smm.exe : force
	gprbuild -Psmm_agg.gpr $(GNATMAKE_ARGS)

test_all_harness.out : test_all_harness.exe smm.exe

VPATH := ../../source
VPATH += ../../test

GNAT_PROJECT := smm_agg.gpr
GPRBUILD_TARGET := $(shell gcc -dumpmachine)

# end of file
