all : books-main.exe tests

install : books-main.exe
	strip -o ~/bin books-main.exe

one : books-main.run

two : RUN_ARGS := stephe csv/jet
two : import_books-main.run

#../../test/test_books-one_harness.adb
t_one : RUN_ARGS := 1
t_one : test_books-one_harness.run

trace :
	addr2line -e books.exe 0x0050E788 0x7C9032A6 0x7C903278 0x7C90E488 0x00437143 0x0043964C 0x63A42F64 0x63A5114E 0x63A52169 0x63A52328 0x61830F23 0x63A46846 0x61813A4F 0x6C364FFA 0x685DFB1F 0x685E0F1B 0x685E11A2 0x61814094 0x00495134 0x00401CA8 0x00401235 0x00401286 0x7C817075

include ../common.make

GNAT_PROJECT := books.gpr

# ADA_PROJECT_PATH is also set by books.prj, but that's not guarranteed to be current
# We are using main_lynx_5 for Ada 2012
export ADA_PROJECT_PATH := $(GDS_ROOT)/main_lynx_5/makerules/
export ADA_PROJECT_PATH := $(ADA_PROJECT_PATH);$(GDS_ROOT)/main_lynx_5/sal/build/x86_gnu_windows_release
export ADA_PROJECT_PATH := $(ADA_PROJECT_PATH);$(GDS_ROOT)/main_lynx_5/sal/build/x86_gnu_windows_gtk_release

# Local Variables:
# eval: (gnat-2012)
# eval: (ada-parse-prj-file "books.prj")
# eval: (ada-select-prj-file "books.prj")
# end:
# end of file
