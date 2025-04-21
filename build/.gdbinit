set args --verbosity=2 compare_playlist protest c:/home/Stephe/smm/spotify_missing_protest.json

set max-value-size unlimited

# unbounded_string_object
define show_unbounded 
  print $arg0.reference.data (1 .. $arg0.reference.last)
end

set print thread-events off
# end of file
