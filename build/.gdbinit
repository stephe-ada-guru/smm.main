# see ~/.gdbinit for common commands

cd /home/stephe/Projects/smm.work_1
set args 1 smm-database_remote-ip-test.adb Test_Sync 1

catch exception 

# unbounded_string_object
define show_unbounded 
  print $arg0.reference.data (1 .. $arg0.reference.last)
end

# JSON_Value
define show_json
  call test_smm.show_json ($arg0)
end
# end of file
