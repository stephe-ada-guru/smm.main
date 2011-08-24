# misc tasks; use Eclipse for most stuff

# push a music file to running android emulator
sdcard:
	adb push "/Projects/Music/Shawn Colvin/These Four Walls/01 - Fill Me Up.mp3" /sdcard/Audio/Vocal/

intent:
	adb shell am start -a android.intent.action.VIEW -c android.intent.category.DEFAULT  -t "audio/x-mpegurl" -d "file:///mnt/sdcard/Audio/vocal.m3u"

# end of file
