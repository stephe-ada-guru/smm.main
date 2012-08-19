all : books-main.exe
all : books-backup.exe
all : books-import-main.exe

#one : books-main.run
one : books-import-main.run

#../../test/test_books-one_harness.adb
t_one : RUN_ARGS := 1
t_one : test_books-one_harness.run

#t_all : GNATMAKE_ARGS = -v
#t_all : GNATMAKE_POST_ARGS = -cargs -v
t_all : RUN_ARGS := 1 -v
t_all : test_books-all_harness.diff

trace :
	addr2line -e books-main.exe  0x00456FC8 0x00428F76 0x0042A9CD 0x0042D96E 0x0042D547 0x00434572 0x00440928 0x00441368 0x0044D87B 0x00402D89 0x004010B9 0x004012A6 0x76A83398 0x77679EF0 0x77679EC3
include ../common.make

GNAT_PROJECT := books.gpr

# ADA_PROJECT_PATH is also set by books.prj, but that's not guarranteed to be current
# We are using main_lynx_5 for Ada 2012
export ADA_PROJECT_PATH := $(GDS_ROOT)/main_lynx_5/makerules/
export ADA_PROJECT_PATH := $(ADA_PROJECT_PATH);$(GDS_ROOT)/main_lynx_5/sal/build/x86_gnu_windows_debug
export ADA_PROJECT_PATH := $(ADA_PROJECT_PATH);$(GDS_ROOT)/main_lynx_5/sal/build/x86_gnu_windows_gtk_debug

# Local Variables:
# eval: (gnat-2012)
# eval: (ada-parse-prj-file "books.prj")
# eval: (ada-select-prj-file "books.prj")
# end:
# end of file
