# gnu make file
# (sal-standard)
# (dvc.stephe)
# (dvc-status "../..")

include ../common.make

export ADA_PROJECT_PATH = $(GDS_ROOT)/main/makerules;$(GDS_ROOT)/main/sal/Build/x86_gnu_windows_release;$(GDS_ROOT)/main/sal/build

# ../../test/test_one_harness.adb
one : test_one_harness.run

install : smm.exe
	strip smm.exe -o ~/bin/smm.exe

trace :
	addr2line -e smm.exe 0x435993 0x4374d2 0x41656b 0x403e34 0x41611b 0x4194c9 0x40170f 0x401235 0x401286 0x7c816fd5


#
#Local Variables:
#eval: (ada-parse-prj-file "smm.prj")
#eval: (ada-select-prj-file "smm.prj")
#End:
# end of file
