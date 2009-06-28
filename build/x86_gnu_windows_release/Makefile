all : dirs books.exe tests

one : books.run

two : RUN_ARGS := stephe csv/jet
two : import_books-main.run

#../../test/test_books-one_harness.adb
t_one : RUN_ARGS := 1
t_one : test_books-one_harness.run

trace :
	addr2line -e books.exe 0x43a65b 0x448d33 0x4496ce 0x4270f8 0x63a42f64 0x63a5114e 0x63a52169 0x63a52328 0x61798815 0x6183eb92 0x63a42f64 0x63a50fd5 0x63a52169 0x63a52328 0x61798815 0x6189b82d 0x63a42f64 0x63a50fd5 0x63a52169 0x63a52328 0x617987f5 0x617994ef 0x61815681 0x63a42f64 0x63a51296 0x63a51f3b 0x63a52328 0x618d4078 0x618135f7 0x61813970 0x439dcf 0x6c364ffa 0x685dfb1f 0x685e0f1b 0x685e11a2 0x61814094 0x44bca9 0x401c03 0x401235 0x401286 0x7c817075

include ../common.make

GNAT_PROJECT := books.gpr

# ADA_PROJECT_PATH is also set by books.prj, but that's not guarranteed to be current
export ADA_PROJECT_PATH := $(GDS_ROOT)/main/makerules/;$(GDS_ROOT)/main/sal/build/x86_gnu_windows_release;../../../gnade/dbi/odbc/;$(GDS_ROOT)/main/sal/build/x86_gnu_windows_gtk_release;$(GTKADA)

# Local Variables:
# eval: (ada-parse-prj-file "books.prj")
# eval: (ada-select-prj-file "books.prj")
# end:
# end of file
