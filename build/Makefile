# build smm and run tests

include rules.make

export ALIRE_BUILD_ARGS ?= --release
#ALIRE_BUILD_ARGS ?= --development

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
	cd ~/Music; $(CURDIR)/smm.exe update "Barry Miles, Eric Claus/Pittsburg 1980/"

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
#End
