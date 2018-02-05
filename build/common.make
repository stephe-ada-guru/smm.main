# common parts of makefiles

all : smm.exe test_all_harness.diff smm.html

tests : test_all_harness.diff

include ../../org.stephe_leake.makerules/common_rules.make
include ../../org.stephe_leake.makerules/gprbuild_rules.make
include ../../org.stephe_leake.makerules/texinfo_rules.make

# smm.adb is not the main program, so we need this rule
smm.exe : force
	gprbuild -p -Psmm.gpr $(GNATMAKE_ARGS) smm-driver

test_all_harness.out : test_all_harness.exe smm.exe

create_test_db :
	mkdir -p tmp/source
	echo "Root = " $(CURDIR) "/tmp/source" > tmp/smm.db

clean ::
	rm -fr tmp

VPATH := ../source
VPATH += ../test

GNAT_PROJECT := smm.gpr
GPRBUILD_TARGET := $(shell gcc -dumpmachine)

# end of file
