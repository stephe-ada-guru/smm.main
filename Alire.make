# Build smm with Alire

# default is development (= debug).
# ALIRE_BUILD_ARGS ?= --release

# Without -q, the linker is very noisy. But it screws up the error outputs!
#ALIRE_ARGS ?= -q

ALIRE_EXEC_DIR := build/bin

STEPHES_ADA_LIBRARY_ALIRE_PREFIX ?= $(CURDIR)/../org.stephe_leake.sal

include $(STEPHES_ADA_LIBRARY_ALIRE_PREFIX)/build/alire_rules.make

vpath %.adb source

all : alire-build install

#install :: server-data
install :: $(HOME)/bin/smm.exe
#install :: $(HOME)/bin/smm-server_driver.exe
#install :: $(HOME)/bin/smm-show_id3.exe

# SERVER_DATA defined in prj-alire.el

server-data :: $(SERVER_DATA)/app.ico
server-data :: $(SERVER_DATA)/liner_notes_icon-desktop.png
server-data :: $(SERVER_DATA)/liner_notes_icon-tablet.png
server-data :: $(SERVER_DATA)/liner_notes_icon-phone.png
server-data :: $(SERVER_DATA)/play_icon-desktop.png
server-data :: $(SERVER_DATA)/play_icon-tablet.png
server-data :: $(SERVER_DATA)/play_icon-phone.png
server-data :: $(SERVER_DATA)/songs.css
server-data :: $(SERVER_DATA)/songs.js

$(SERVER_DATA)/% : source/%
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
	cd /Projects/Music; $(CURDIR)/$(ALIRE_EXEC_DIR)/smm.exe --db=/Projects/smm.main/smm_new.db --verbosity=$(VERBOSITY) update_playlist /tmp/vocal.m3u vocal 20 --replace

t2 : $(ALIRE_EXEC_DIR)/debug.exe
	$(ALIRE_EXEC_DIR)/debug.exe ~/smm/smm.db vocal

t3 : $(ALIRE_EXEC_DIR)/smm.exe
	$(ALIRE_EXEC_DIR)/smm.exe history

.PHONEY : t1 t2

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-alire.el"))
# End:
# end of file
