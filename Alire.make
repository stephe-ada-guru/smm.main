# Build smm with Alire

#default is debug
#ALIRE_BUILD_ARGS :? --release

STEPHES_ADA_LIBRARY_ALIRE_PREFIX ?= $(CURDIR)/../org.stephe_leake.sal

include $(STEPHES_ADA_LIBRARY_ALIRE_PREFIX)/build/alire_rules.make

# if 'all' target fails due to alire stuff, use 'alire-build'. otherwise, this is faster.

all : alr.env force
	source ./alr.env; /mingw64/bin/gprbuild -P build/smm_alire.gpr

install :: server-data
install :: c:/home/stephe/bin/smm.exe
install :: c:/home/stephe/bin/smm-server_driver.exe
install :: c:/home/stephe/bin/smm-show_id3.exe

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
c:/home/stephe/bin/% : build/obj/development/%
	cp $^ $@

obj/development/smm.exe : alr.env force
	. ./alr.env; /mingw64/bin/gprbuild -P build/smm_alire.gpr smm-driver.adb

clean : alire-clean
	rm -f alr.env

# this also cleans dependencies
really-clean : clean
	rm -rf ~/.config/alire/cache/builds

t1 : VERBOSITY ?= 0
t1 : obj/development/smm.exe
	build/obj/development/smm.exe --verbosity=$(VERBOSITY) compare_playlist protest c:/home/Stephe/smm/spotify_missing_protest.json

.PHONEY : t1

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-alire.el"))
# End:
# end of file
