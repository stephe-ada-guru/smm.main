dirs : objects

objects :
	mkdir objects

books.exe : books-main.exe

tests : test_books-all_harness.diff

empty_database_test :
	 mysql -u stephe --execute="source ../../test/delete_database_test_mysql.sql"
	 mysql -u stephe --execute="source ../../test/create_database_test_mysql.sql"
	 mysql -u stephe --execute="source ../../src/create_schema_mysql.sql" test

VPATH = ../../src
VPATH += ../../test
VPATH += ../../../gnade-src-1.5.3b/dbi/odbc

include $(GDS_ROOT)/main/makerules/gnat_project_rules.make
include $(GDS_ROOT)/main/makerules/common_rules.make

# end of file
