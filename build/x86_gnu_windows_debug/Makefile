all : books-main.exe

one : books-main.run

#../../test/test_books-one_harness.adb
t_one : RUN_ARGS := 1
t_one : test_books-one_harness.run

#t_all : GNATMAKE_ARGS = -v
#t_all : GNATMAKE_POST_ARGS = -cargs -v
t_all : RUN_ARGS := 1 -v
t_all : test_books-all_harness.diff

trace :
	addr2line -e test_books-one_harness.exe 0x467994 0x46afeb 0x475121 0x4929eb 0x43cf13 0x43f5de 0x63a42f64 0x63a5114e 0x63a52169 0x63a52328 0x61798815 0x61799a87 0x61799afc 0x61815681 0x63a42f64 0x63a51296 0x63a51f3b 0x63a52328 0x618d4078 0x618136ae 0x61813970 0x467b1d 0x6c364ffa 0x685dfb1f 0x685e0f1b 0x685e11a2 0x61814094 0x49dc5b 0x4ea584 0x7c80b727
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
