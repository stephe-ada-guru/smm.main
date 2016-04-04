tests : test_books-all_harness.diff

# the 'select' statement makes sqlite3 terminate instead of entering interactive mode; -batch doesn't help
create_books_db : create_schema.sql
	sqlite3 -echo -init ../../source/create_schema.sql ~/.books/books.db "select * from Author"

backup : backup.sql
	cd ../../backup; sqlite3 -echo -init ../source/backup.sql ~/.books/books.db "select * from Author"

create_books_import_db : create_schema.sql
	sqlite3 -echo -init ../../source/create_schema.sql books_import.db "select * from Author"

clean_books_import_db :
	rm books_import.db

VPATH = ../../source
VPATH += ../../test

include ../../../org.stephe_leake.makerules/gprbuild_rules.make
include ../../../org.stephe_leake.makerules/common_rules.make

# end of file
