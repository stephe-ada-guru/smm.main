# gnu make file
# (dvc-status "../..")

include ../common.make

# ../../test/test_one_harness.adb
# ../../test/test_all_harness.adb
t_one : test_one_harness.run
t_all : test_all_harness.diff

install : ~/bin/smm.exe

# don't strip, so stack traceback is useful on errors
~/bin/smm.exe : smm.exe
	cp smm.exe ~/bin/smm.exe

trace :
	addr2line -e smm.exe 0x435993 0x4374d2 0x41656b 0x403e34 0x41611b 0x4194c9 0x40170f 0x401235 0x401286 0x7c816fd5


#
#Local Variables:
#eval: (ada-parse-prj-file "smm.prj")
#eval: (ada-select-prj-file "smm.prj")
#End:
# end of file
