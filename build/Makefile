# gnu make file

include rules.make

# ../test/test_one_harness.adb
# ../test/test_all_harness.adb
export Standard_Common_Build := Debug
#t_one : RUN_ARGS := 1
#t_one : smm-server_driver.exe
t_one : test_one_harness.run

#t_all : export Standard_Common_Build := Debug
t_all : smm-server_driver.exe test_all_harness.run

one-clean :
	rm -f smm_config.db

one : export Standard_Common_Build := Debug
one : debug.exe
	./debug.exe "d:/music/beth nielsen chapman/beth nielsen chapman/that's the easy part.mp3"

# This server can't be seen from phone, tablet (not clear why); use
# ~/bin/music-server-work_1.sh
run-server : smm-server_driver.exe
	./smm-server_driver.exe smm_server_devel.config

convert-main : config_to_sqlite.exe | c:/home/stephe/smm/smm_work_1.db
	./$< ~/smm/smm_server.config c:/home/stephe/smm/smm_work_1.db

DATE := $(shell date +%Y-%m-%d)

histogram : ~/bin/smm.exe
	cd d:/Music; smm history > status-$(DATE).text
	cd d:/Music; gnuplot vocal_histogram.gnuplot > vocal_histogram.png
	cd d:/Music; gnuplot instrumental_histogram.gnuplot > instrumental_histogram.png

# check : export Standard_Common_Build = Debug
check : smm.exe
	cd d:/Music; $(CURDIR)/smm check > missing.log

install : server-data c:/home/stephe/bin/smm.exe c:/home/stephe/bin/smm-server_driver.exe

.PHONY : server-data play-icons

SERVER_DATA := /d/Music/server_data

server-data :: $(SERVER_DATA)/app_icon.png
server-data :: $(SERVER_DATA)/liner_notes_icon-28x37.png
server-data :: $(SERVER_DATA)/liner_notes_icon-112x148.png
server-data :: $(SERVER_DATA)/liner_notes_icon-252x333.png
server-data :: $(SERVER_DATA)/play_icon-19x19.png
server-data :: $(SERVER_DATA)/play_icon-76x76.png
server-data :: $(SERVER_DATA)/play_icon-171x171.png
server-data :: $(SERVER_DATA)/search.html
server-data :: $(SERVER_DATA)/search.css
server-data :: $(SERVER_DATA)/songs.css
server-data :: $(SERVER_DATA)/songs.js

# svg image is 0.2"x0.2"; correct size for desktop screen, 96 dpi = 19x19 pixels.
# tablet : * 4 = 76
# phone  : * 9 = 171
# Similarly for liner_notes
# command line inkscape doesn't work (it ignores scaling options)
# command line imageMagick delegates to inkscape for svg
# use GUI inkscape

$(SERVER_DATA)/% : ../source/%
	cp $^ $@

# don't strip, so stack traceback is useful on errors
c:/home/stephe/bin/% : %
	cp $^ $@

trace :
	addr2line -e smm.exe 0x435993 0x4374d2 0x41656b 0x403e34 0x41611b 0x4194c9 0x40170f 0x401235 0x401286 0x7c816fd5


#
#Local Variables:
#eval: (load-file "prj.el")
#End:
# end of file
