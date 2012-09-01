books.exe : books-main.exe

tests : test_books-all_harness.diff

# the 'select' statement makes sqlite3 terminate instead of entering interactive mode
create_books_db :
	sqlite3 -echo -init ../../src/create_schema.sql ~/.books/books.db "select * from Author"

clean_books_db :
	rm ~/.books/books.db

empty_database_test :
	 mysql -u stephe --execute="source ../../test/recreate_database_test_mysql.sql"

test_data :
	mysql -u stephe --execute="source ../../test/create_test_data_mysql.sql"

VPATH = ../../source
VPATH += ../../test
VPATH += ../../../gnade-src-1.5.3b/dbi/odbc

include $(GDS_ROOT)/main/makerules/gnat_project_rules.make
include $(GDS_ROOT)/main/makerules/common_rules.make

# end of file
