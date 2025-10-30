set args --verbosity=2 compare_playlist best html /Projects/web/stephe-leake/best_stuff-music.html

# unbounded_string_object
define show_unbounded 
  print $arg0.reference.data (1 .. $arg0.reference.last)
end
# end of file
