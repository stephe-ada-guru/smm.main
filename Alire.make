# Build smm with Alire

# ALIRE_BUILD_ARGS ?= --release
# also -gnatdp -gnatdV -gnatdi
ALIRE_BUILD_ARGS ?= --development
#-- -v -gnatdi smm-database-diff-test_apply.adb

#ALIRE_ARGS ?= -v

#GPRBUILD_ARGS = -v

ALIRE_EXEC_DIR := $(CURDIR)/build/bin

STEPHES_ADA_LIBRARY_ALIRE_PREFIX ?= $(CURDIR)/../org.stephe_leake.sal

include $(STEPHES_ADA_LIBRARY_ALIRE_PREFIX)/build/alire_rules.make

vpath %.adb source test
vpath %.svg source

all : alire-build install

install : server-data
install : $(HOME)/.local/bin/smm.exe

# These require sudo, which emacs compile doesn't handle properly
# install : /usr/lib/cgi-bin/smm-server_driver.exe
# install : /usr/lib/cgi-bin/smm

# SERVER_DATA defined in prj-alire.el

server-data : $(SERVER_DATA)/liner_notes_icon-desktop.png
server-data : $(SERVER_DATA)/liner_notes_icon-tablet.png
server-data : $(SERVER_DATA)/liner_notes_icon-phone.png
server-data : $(SERVER_DATA)/play_icon-desktop.png
server-data : $(SERVER_DATA)/play_icon-tablet.png
server-data : $(SERVER_DATA)/play_icon-phone.png
server-data : $(SERVER_DATA)/songs.css
server-data : $(SERVER_DATA)/songs.js

$(SERVER_DATA)/liner_notes_icon-desktop.png $(SERVER_DATA)/liner_notes_icon-tablet.png $(SERVER_DATA)/liner_notes_icon-phone.png : liner_notes_icon.svg
	rsvg-convert -h 50 -a $< > $@

$(SERVER_DATA)/play_icon-desktop.png $(SERVER_DATA)/play_icon-tablet.png $(SERVER_DATA)/play_icon-phone.png : play_icon.svg
	rsvg-convert -h 10 -a $< > $@

$(SERVER_DATA)/app_icon.png : app_icon.svg
	rsvg-convert -h 20 -a $< > $@

$(SERVER_DATA)/% : source/%
	cp $^ $@

# script used in URLs.
/usr/lib/cgi-bin/smm : source/smm
	sudo cp $^ $@

# don't strip, so stack traceback is useful on errors
$(HOME)/.local/bin/% : $(ALIRE_EXEC_DIR)/%
	cp $^ $@

modify : $(ALIRE_EXEC_DIR)/smm-database-modify_schema.exe modify-clean smm_new.db
	$(ALIRE_EXEC_DIR)/smm-database-modify_schema.exe /var/www/html/music_server_data/smm.db smm_new.db

modify-clean :
	rm -rf smm_new.db

smm%.db : source/create_schema.sql
	sqlite3 -init $< $@ ".quit"

clean : alire-clean

# this also cleans dependencies
really-clean : clean
	rm -rf ~/.config/alire/cache/builds

# Source file is smm-driver.adb, so alire_rules %.exe doesn't match
$(ALIRE_EXEC_DIR)/smm.exe : force
	alr $(ALIRE_ARGS) build $(ALIRE_BUILD_ARGS) -- $(GPRBUILD_ARGS) smm-driver.adb

empty_database_test_1 : source/create_schema.sql
	rm -f smm_test_1.db smm_test_1.config
	sqlite3 -echo -init source/create_schema.sql smm_test_1.db ".quit"
	echo "Database_File=smm_test_1.db" > smm_test_1.config
	echo "Server_IP=$(SERVER_IP)" >> smm_test_1.config
	echo "Server_Port=$(SERVER_PORT)" >> smm_test_1.config

empty_database_test_2 : source/create_schema.sql
	rm -f smm_test_2.db smm_test_2.config
	sqlite3 -echo -init source/create_schema.sql smm_test_2.db ".quit"
	echo "Database_File=smm_test_2.db" > smm_test_2.config
	echo "Server_IP=$(SERVER_IP)" >> smm_test_2.config
	echo "Server_Port=$(SERVER_PORT)" >> smm_test_2.config


t1 : VERBOSITY ?= 0
t1 : $(ALIRE_EXEC_DIR)/smm.exe
	cd /var/www/html/Music; $(ALIRE_EXEC_DIR)/smm.exe --verbosity=$(VERBOSITY) --max_errors=5 compare_phone /tmp/phone_music.log

# method file parameters
t2 : $(ALIRE_EXEC_DIR)/debug_web_server.exe
	$(ALIRE_EXEC_DIR)/debug_web_server.exe GET "id" "file=Christine%20Lavin/Happydance%20of%20the%20Zenophobe/01%20The%20Most%20Polite%20City%20in%20the%20World.mp3"

# VERBOSITY="1 smm-database-diff-test_compute.adb Collisions 2"
t3 : $(ALIRE_EXEC_DIR)/test_one_harness.exe
	$(ALIRE_EXEC_DIR)/test_one_harness.exe $(VERBOSITY)

t_all : $(ALIRE_EXEC_DIR)/test_all_harness.exe
	$(ALIRE_EXEC_DIR)/test_all_harness.exe $(VERBOSITY)

.PHONEY : t1 t2 t3 t_all empty_database_test_1 empty_database_test_2

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-alire.el"))
# End:
# end of file
