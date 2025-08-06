# Build smm with Alire

# default is development (= debug). Without --quiet, the linker is very noisy.
ALIRE_BUILD_ARGS :? --release --quiet

STEPHES_ADA_LIBRARY_ALIRE_PREFIX ?= $(CURDIR)/../org.stephe_leake.sal

include $(STEPHES_ADA_LIBRARY_ALIRE_PREFIX)/build/alire_rules.make

all : alire-build install

install :: server-data
install :: $(HOME)/bin/smm.exe
install :: $(HOME)/bin/smm-server_driver.exe
install :: $(HOME)/bin/smm-show_id3.exe

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
$(HOME)/bin/% : build/bin/%
	cp $^ $@

build/obj/development/smm.exe : alr.env force
	. ./alr.env; /mingw64/bin/gprbuild -P build/smm_alire.gpr smm-driver.adb

build/obj/development/test_one_harness.exe : alr.env force
	source ./alr.env; /mingw64/bin/gprbuild -P build/smm_test.gpr test_one_harness.adb

clean : alire-clean
	rm -f alr.env

# this also cleans dependencies
really-clean : clean
	rm -rf ~/.config/alire/cache/builds

t1 : VERBOSITY ?= 0
t1 : build/obj/development/smm.exe
	build/obj/development/smm.exe --verbosity=$(VERBOSITY) compare_playlist protest $(HOME)/smm/spotify_missing_protest.json

t2 : build/obj/development/test_one_harness.exe
	cd build; obj/development/test_one_harness.exe 1 test_server.adb ""

.PHONEY : t1 t2

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-alire.el"))
# End:
# end of file
