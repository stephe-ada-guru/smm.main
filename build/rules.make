# common parts of makefiles

all : smm.exe
all : smm-server_driver.exe
all : config_to_sqlite.exe
all : test_all_harness.diff
all : smm.html

tests : test_all_harness.diff

include ../../org.stephe_leake.makerules/common_rules.make
include ../../org.stephe_leake.makerules/gprbuild_rules.make
include ../../org.stephe_leake.makerules/texinfo_rules.make

# smm.adb is not the main program, so we need this rule
smm.exe : force
	gprbuild -p -Psmm.gpr smm-driver

test_all_harness.out : test_all_harness.exe smm-server_driver.exe smm.exe

test_all_harness.exe : GNAT_PROJECT := smm_test.gpr
test_one_harness.exe : GNAT_PROJECT := smm_test.gpr

create_test_db :
	mkdir -p tmp/source
	echo "Root = " $(CURDIR) "/tmp/source" > tmp/smm.db

smm%.db : ../source/create_schema.sql
	sqlite3 -init $< $@ ".quit"

sqlite-clean :
	rm -f smm*.db
	rm -f tmp/smm*.db

clean :: sqlite-clean
	rm -fr tmp

VPATH := ../source
VPATH += ../test

GNAT_PROJECT := smm.gpr
GPRBUILD_TARGET := $(shell gcc -dumpmachine)

# end of file
