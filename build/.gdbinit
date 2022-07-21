set args "d:/Music/Beth Wood/Love is Onto You/01. Love Keeps the Flame.m4a"

break C:\Projects\smm.main\source\smm-m4a.adb:296

command 1
p frame.id
show_unbounded frame.data
end

catch excep
set varsize-limit 0


# unbounded_string_object
define show_unbounded 
  print $arg0.reference.data (1 .. $arg0.reference.last)
end

set print thread-events off
# end of file
