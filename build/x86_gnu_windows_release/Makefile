all : books-main.exe
# all : books-backup.exe not up to date
all : books-import-main.exe
# all : tests not up to date

install : books-main.exe
	strip -o ~/bin/books-main.exe books-main.exe

one_check : books-main.check one
one : books-main.run
#one : RUN_ARGS := books.config ../../backup/
#one : books-import-main.run

#../../test/test_books-one_harness.adb
t_one : RUN_ARGS := 1
t_one : test_books-one_harness.run

#t_all : GNATMAKE_ARGS = -v
#t_all : GNATMAKE_POST_ARGS = -cargs -v
t_all : RUN_ARGS := 1 -v
t_all : test_books-all_harness.diff

trace :
	addr2line -e books-main.exe  0x44b572 0x41302e 0x414f5f 0x63a43e94 0x63a53b31 0x63a55339 0x63a554b8 0x61799fd5 0x6184dcb6 0x63a43e94 0x63a531b4 0x63a55339 0x63a554b8 0x61799fd5 0x4358e9 0x441a58 0x442498 0x44ea23 0x402dc3 0x4010b9 0x4012a6 0x768b3398 0x77859ef0 0x77859ec3

include ../common.make

GNAT_PROJECT    := books_agg.gpr
GPRBUILD_TARGET := x86-windows

# Local Variables:
# eval: (gnat-2012)
# eval: (ada-parse-prj-file "books.prj")
# eval: (ada-select-prj-file "books.prj")
# end:
# end of file
