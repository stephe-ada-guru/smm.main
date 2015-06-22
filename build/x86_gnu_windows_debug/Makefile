# gnu make file
# (dvc-status "../..")

include ../common.make

# ../../test/test_one_harness.adb
t_one : test_one_harness.run
t_all : test_all_harness.diff

two : smm.exe
	./smm.exe --max-song-count=200 download_playlist instrumental "E:/Audio/" "E:/Android/data/org.stephe_leake.android.stephes_music/cache/"

trace :
	addr2line -e smm.exe 0x435993 0x4374d2 0x41656b 0x403e34 0x41611b 0x4194c9 0x40170f 0x401235 0x401286 0x7c816fd5


#
#Local Variables:
#eval: (ada-parse-prj-file "smm.prj")
#eval: (ada-select-prj-file "smm.prj")
#End:
# end of file
