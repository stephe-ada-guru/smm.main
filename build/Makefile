n# obsolete; move rules to ../alire.make

include rules.make

export Standard_Common_Build := Debug

# ../test/test_one_harness.adb
# ../test/test_all_harness.adb
#t_one : RUN_ARGS := 1
#t_one : smm-server_driver.exe
t_one : test_one_harness.run

t_all : smm.exe smm-server_driver.exe test_all_harness.run

one-clean :
	rm -f smm_config.db

one : smm.exe
	cd d:/Music; $(CURDIR)/smm.exe update "Barry Miles, Eric Claus/Pittsburg 1980/"

two : smm-server_driver.exe
#	cd d:/Music; $(CURDIR)/smm.exe update "Beth Wood/20-20-2017/"

# This server can't be seen from phone, tablet (not clear why)
run-server : server-data smm-server_driver.exe
	./smm-server_driver.exe smm_server_devel.config

DATE := $(shell date +%Y-%m-%d)

histogram : c:/home/stephe/bin/smm.exe
	cd d:/Music; smm history > status-$(DATE).text
	cd d:/Music; gnuplot vocal_histogram.gnuplot > vocal_histogram.png
	cd d:/Music; gnuplot instrumental_histogram.gnuplot > instrumental_histogram.png

check : smm.exe
	cd d:/Music; $(CURDIR)/smm check > missing.log

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
c:/home/stephe/bin/% : %
	cp $^ $@

trace :
	addr2line -e smm.exe 0x435993 0x4374d2 0x41656b 0x403e34 0x41611b 0x4194c9 0x40170f 0x401235 0x401286 0x7c816fd5


#
#Local Variables:
#eval: (load-file "prj.el")
#End:
# end of file
