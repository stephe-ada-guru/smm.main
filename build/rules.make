# obsolete; move rules to ../Alire.make

all : obj/development/smm.exe
all : obj/development/smm-server_driver.exe
all : test_all_harness.diff

tests : test_all_harness.diff

include ../../org.stephe_leake.makerules/common_rules.make
include ../../org.stephe_leake.makerules/gprbuild_rules.make
include ../../org.stephe_leake.makerules/texinfo_rules.make

obj/development/% : alr.env force
	. ./alr.env; gprbuild -P smm.gpr

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
