# top level Makefile to build Stephe's Music Player app

.PHONY : force

# lint looks for compiled classes, to it has to run after the compiler
all : compile lint

compile : force
	cd ..; ant release

lint :
	cmd.exe /c lint ..

clean :
	rm -rf ../bin/* ../gen/*

source-clean :
	find .. -name "*~" -print -delete

tag :
	mtn tag h:org.stephe_leake.music_player.java android-2.3

## test targets

# emulator from SDK 22.0.5 doesn't run on Windows 7
#install-emulator: ANDROID_DEVICE := -e
#install-emulator: install

# assumes extSdCard is mounted via webDAV as y:
install-pda-webdav-apk : all install-pda-webdav
install-pda-webdav :
	cp ../bin/org.stephe_leake.android.stephes_music.4.1-release.apk /cygdrive/y/Download

# assumes PDA is running sshDroid server, ssh key is active
PDA_IP := 192.168.1.66
#install-pda-ssh : all
install-pda-ssh :
	scp -P 2222 ../bin/org.stephe_leake.android.stephes_music.4.1-release.apk root@$(PDA_IP):/storage/sdcard0/Download

# ethernet address can change
connect-tv:
	adb connect 192.168.0.101

install: all
	adb install -r ../bin/org.stephe_leake.android.stephes_music.sdk_20-release.apk

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
