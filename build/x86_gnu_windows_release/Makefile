all : dirs books.exe

one : books.run

include ../common.make

# GtkAda library was installed as relocatable; not clear how to also install static!
LIBRARY_TYPE := relocatable
GNAT_PROJECT := books.gpr

# ADA_PROJECT_PATH is also set by books.prj, but that's not guarranteed to be current
export ADA_PROJECT_PATH := $(GDS_ROOT)/main/makerules/;$(GDS_ROOT)/main/sal/build/x86_gnu_windows_release;../../../gnade/dbi/odbc/;$(GDS_ROOT)/main/sal/build/x86_gnu_windows_gtk_release;/Apps/GNAT-6.2.1/GtkAda/lib/gnat

# Local Variables:
# eval: (ada-parse-prj-file "books.prj")
# eval: (ada-select-prj-file "books.prj")
# end:
# end of file
