tests : test_books-all_harness.diff

# the 'select' statement makes sqlite3 terminate instead of entering interactive mode
create_books_db :
	sqlite3 -echo -init ../../src/create_schema.sql ~/.books/books.db "select * from Author"

clean_books_db :
	rm ~/.books/books.db

VPATH = ../../source
VPATH += ../../test

include ../../../org.stephe_leake.makerules/gprbuild_rules.make
include ../../../org.stephe_leake.makerules/common_rules.make

# end of file
