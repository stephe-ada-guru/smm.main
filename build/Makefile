# top level Makefile to build Stephe's Music Player app

.PHONY : force

all : force
	cd ..; ant release

clean :
	rm -rf ../bin/* ../gen/*

source-clean :
	find .. -name "*~" -print -delete

## test targets

install-emulator: ANDROID_DEVICE := -e
install-emulator: install

# push music files to running android emulator
sdcard :
	adb shell echo "Vocal/01 - Fill Me Up.mp3" > /sdcard/Audio/vocal.m3u
	adb shell echo "Vocal/02 - These Four Walls.mp3" >> /sdcard/Audio/vocal.m3u
	adb push "/Projects/Music/Shawn Colvin/These Four Walls/01 - Fill Me Up.mp3" /sdcard/Audio/Vocal/
	adb push "/Projects/Music/Shawn Colvin/These Four Walls/02 - These Four Walls.mp3" /sdcard/Audio/Vocal/
	adb shell echo "Instrumental/Slow Circle 2.mp3" > /sdcard/Audio/instrumental.m3u
	adb shell echo "Instrumental/Coilsfield House.mp3" >> /sdcard/Audio/instrumental.m3u
	adb push "/Projects/Music/Alex de Grassi/Slow Circle/Slow Circle 2.mp3" /sdcard/Audio/Instrumental/
	adb push "/Projects/Music/Joemy Wilson/Celtic Dreams/Coilsfield House.mp3" /sdcard/Audio/Instrumental/

app-intent-main :
	adb shell am start -n org.stephe_leake.android.stephes_music.activity -a android.intent.action.MAIN -c android.intent.category.LAUNCHER

app-intent-view :
	adb shell am start -n org.stephe_leake.android.stephes_music.activity -a android.intent.action.VIEW -c android.intent.category.DEFAULT  -t "audio/x-mpegurl" -d "file:///mnt/sdcard/Audio/vocal.m3u"

#Local Variables:
#eval: (c-parse-prj-file "app.prj")
#End:
# end of file
