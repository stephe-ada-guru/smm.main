dirs : objects

objects :
	mkdir objects

books.exe : books-main.exe

VPATH = ../../src
VPATH += ../../../gnade-src-1.5.3b/dbi/odbc

include $(GDS_ROOT)/main/makerules/gnat_project_rules.make
include $(GDS_ROOT)/main/makerules/common_rules.make

# end of file
