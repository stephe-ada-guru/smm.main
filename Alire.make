# Build smm with Alire

# ALIRE_BUILD_ARGS ?= --release
ALIRE_BUILD_ARGS ?= --development

# Without -q, the linker is very noisy. But it screws up the error outputs!
#ALIRE_ARGS ?= -q

ALIRE_EXEC_DIR := $(CURDIR)/build/bin

STEPHES_ADA_LIBRARY_ALIRE_PREFIX ?= $(CURDIR)/../org.stephe_leake.sal

include $(STEPHES_ADA_LIBRARY_ALIRE_PREFIX)/build/alire_rules.make

vpath %.adb source
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

modify : $(ALIRE_EXEC_DIR)/modify_schema.exe smm_new.db
	$(ALIRE_EXEC_DIR)/modify_schema.exe /var/www/html/music_server_data/smm.db smm_new.db

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
	cd /var/www/html/Music; $(ALIRE_EXEC_DIR)/smm.exe --verbosity=$(VERBOSITY) --max_errors=5 compare_phone /tmp/phone_music.log

t2 : $(ALIRE_EXEC_DIR)/debug_web_server.exe
	$(ALIRE_EXEC_DIR)/debug_web_server.exe "API=2&category=instrumental&count=80&new_count=26&over_select_ratio=1.1&record_downloaded=true"

.PHONEY : t1 t2

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-alire.el"))
# End:
# end of file
