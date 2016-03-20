# common parts of makefiles

all : smm.exe test_all_harness.diff smm.html

tests : test_all_harness.diff

include ../../org.stephe_leake.makerules/common_rules.make
include ../../org.stephe_leake.makerules/gprbuild_rules.make
include ../../org.stephe_leake.makerules/texinfo_rules.make

# smm.adb is not the main program, so we need this rule
smm.exe : force
	gprbuild -p -Psmm_agg.gpr $(GNATMAKE_ARGS)

test_all_harness.out : test_all_harness.exe smm.exe

VPATH := ../source
VPATH += ../test

GNAT_PROJECT := smm_agg.gpr
GPRBUILD_TARGET := $(shell gcc -dumpmachine)

# end of file
