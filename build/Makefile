# build smm and run tests

include rules.make

# ALIRE_BUILD_PROFILE must be changed in Emacs so gpr_query sees it; see prj.el
#export ALIRE_BUILD_PROFILE := release
#export ALIRE_BUILD_PROFILE := development
ALIRE_BUILD_ARGS := "--"$(ALIRE_BUILD_PROFILE)

ALIRE_EXEC_DIR := $(CURDIR)/bin

all : build-executables test_all_harness.diff

install : server-data
install : $(HOME)/.local/bin/smm.exe
install : $(HOME)/.local/bin/smm-db_sync_server.exe

# These require sudo, which emacs compile doesn't handle properly
# install : /usr/lib/cgi-bin/smm-server_driver.exe
# install : /usr/lib/cgi-bin/smm

# SERVER_DATA defined in prj.el
server-data : $(SERVER_DATA)/liner_notes_icon-desktop.png
server-data : $(SERVER_DATA)/liner_notes_icon-tablet.png
server-data : $(SERVER_DATA)/liner_notes_icon-phone.png
server-data : $(SERVER_DATA)/play_icon-desktop.png
server-data : $(SERVER_DATA)/play_icon-tablet.png
server-data : $(SERVER_DATA)/play_icon-phone.png
server-data : $(SERVER_DATA)/songs.css
server-data : $(SERVER_DATA)/songs.js

update-db_sync_server : bin/smm-db_sync_server.exe
	~/bin/kill-music_db_sync.sh
	cp bin/smm-db_sync_server.exe $(HOME)/.local/bin/smm-db_sync_server.exe
	~/bin/music-db_sync_server.sh

tests : test_all_harness.diff

build-executables : force
	cd ..; alr build $(ALIRE_BUILD_ARGS)

# ../test/test_one_harness.adb
# ../test/test_all_harness.adb
#t_one : RUN_ARGS := 1
#t_one : smm-server_driver.exe
t_one : test_one_harness.run

t_all : build-executables test_all_harness.run

one-clean :
	rm -f smm_config.db

one : build-executables
	bin/smm.exe --max_errors=5 compare_playlist best spotify /home/stephe/smm/spotify_missing_best.json

DATE := $(shell date +%Y-%m-%d)

histogram : build-executables
	cd ~/Music; smm history > status-$(DATE).text
	cd ~/Music; gnuplot vocal_histogram.gnuplot > vocal_histogram.png
	cd ~/Music; gnuplot instrumental_histogram.gnuplot > instrumental_histogram.png

check : build-executables
	cd ~/Music; $(CURDIR)/smm check > missing.log

# svg image is correct size for desktop screen, 96 dpi
# tablet : * 4 = 384 dpi
# phone  : * 9 = 864 dpi
# Similarly for liner_notes
# command line inkscape doesn't work (it ignores scaling options)
# command line imageMagick delegates to inkscape for svg
# use GUI inkscape

$(SERVER_DATA)/% : ../source/%
	cp $^ $@

# don't strip, so stack traceback is useful on errors
~/home/stephe/bin/% : bin/%
	cp $^ $@

#
#Local Variables:
#eval: (load-file "prj.el")
#End:
