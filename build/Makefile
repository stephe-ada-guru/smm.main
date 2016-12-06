# gnu make file

include common.make

# ../test/test_one_harness.adb
# ../test/test_all_harness.adb
#t_one : export SMM_Build := Debug
t_one : RUN_ARGS := 2
t_one : smm-server_driver.exe
t_one : test_one_harness.run
#t_all : export SMM_Build := Debug
t_all : smm-server_driver.exe test_all_harness.diff

install : ~/bin/smm.exe ~/bin/smm-server_driver.exe

# don't strip, so stack traceback is useful on errors
~/bin/% : %
	cp $^ $@

trace :
	addr2line -e smm.exe 0x435993 0x4374d2 0x41656b 0x403e34 0x41611b 0x4194c9 0x40170f 0x401235 0x401286 0x7c816fd5


#
#Local Variables:
#eval: (load-file "prj.el")
#End:
# end of file
