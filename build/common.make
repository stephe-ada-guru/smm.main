# common parts of makefiles

all : smm.exe test_all_harness.diff smm.html

include $(GDS_ROOT)/main/makerules/common_rules.make
include $(GDS_ROOT)/main/makerules/gnat_project_rules.make
include $(GDS_ROOT)/main/makerules/texinfo_rules.make

# smm.adb is not the main program, so we need this rule
smm.exe : force
	gnatmake -p -k -Psmm.gpr $(GNATMAKE_ARGS)

test_all_harness.out : test_all_harness.exe smm.exe

VPATH := ../../source
VPATH += ../../test

GNAT_PROJECT = smm.gpr

# end of file
