# Build smm with Alire

# ALIRE_BUILD_ARGS ?= --release
ALIRE_BUILD_ARGS ?= --development

# Without -q, the linker is very noisy. But it screws up the error outputs!
#ALIRE_ARGS ?= -q

ALIRE_EXEC_DIR := build/bin

STEPHES_ADA_LIBRARY_ALIRE_PREFIX ?= $(CURDIR)/../org.stephe_leake.sal

include $(STEPHES_ADA_LIBRARY_ALIRE_PREFIX)/build/alire_rules.make

vpath %.adb source
vpath %.svg source

all : alire-build install

install : server-data
install : $(HOME)/bin/smm.exe
install : /usr/lib/cgi-bin/smm-server_driver.exe
install : /usr/lib/cgi-bin/smm
#install : $(HOME)/bin/smm-show_id3.exe

# SERVER_DATA defined in prj-alire.el

server-data : $(SERVER_DATA)/app_icon.png
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

/usr/lib/cgi-bin/smm : source/smm
	cp $^ $@

/usr/lib/cgi-bin/smm-server_driver.exe : $(ALIRE_EXEC_DIR)/smm-server_driver.exe
	cp $^ $@

# don't strip, so stack traceback is useful on errors
$(HOME)/bin/% : $(ALIRE_EXEC_DIR)/%
	cp $^ $@

modify : $(ALIRE_EXEC_DIR)/modify_schema.exe smm_new.db
	$(ALIRE_EXEC_DIR)/modify_schema.exe $(HOME)/smm/smm.db smm_new.db

smm%.db : source/create_schema.sql
	sqlite3 -init $< $@ ".quit"

clean : alire-clean

# this also cleans dependencies
really-clean : clean
	rm -rf ~/.config/alire/cache/builds

# Source file is smm-driver.adb, so alire_rules %.exe doesn't match
$(ALIRE_EXEC_DIR)/smm.exe : force
	alr $(ALIRE_ARGS) build $(ALIRE_BUILD_ARGS) -- $(GPRBUILD_ARGS) smm-driver.adb

t1 : VERBOSITY ?= 0
t1 : $(ALIRE_EXEC_DIR)/smm.exe
	$(ALIRE_EXEC_DIR)/smm.exe --verbosity=$(VERBOSITY) --max_errors=6 compare_playlist best html /Projects/web/stephe-leake/best_stuff-music.html

t2 : $(ALIRE_EXEC_DIR)/debug.exe
	$(ALIRE_EXEC_DIR)/debug.exe

.PHONEY : t1 t2

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-alire.el"))
# End:
# end of file
