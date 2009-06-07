all : dirs books.exe

one : books.run

two : RUN_ARGS := stephe ../x86_gnu_windows_release/csv/jet
two : import_books-main.run

include ../common.make

GNAT_PROJECT := books.gpr

# ADA_PROJECT_PATH is also set by books.prj, but that's not guarranteed to be current
export ADA_PROJECT_PATH := $(GDS_ROOT)/main/makerules/;$(GDS_ROOT)/main/sal/build/x86_gnu_windows_debug;../../../gnade/dbi/odbc/;$(GDS_ROOT)/main/sal/build/x86_gnu_windows_gtk_debug;/Apps/GNAT-6.2.1/GtkAda/lib/gnat

# Local Variables:
# eval: (ada-parse-prj-file "books.prj")
# eval: (ada-select-prj-file "books.prj")
# end:
# end of file
