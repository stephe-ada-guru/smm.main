# top level Makefile to build Stephe's Music Player app

# must match app/build.gradle versionName, versionCode
VERSION := 16

.PHONY : force

all : build

# compiles app and runs unit tests
#
#  --info gives more detail, --stacktrace gives error stack trace
# 'gradle tasks' shows available tasks
build : force
	gradle --daemon build

# just compile tasks:
# compileDebugAndroidTestSources
# compileDebugSources
# compileDebugUnitTestSources
# compileReleaseSources
# compileReleaseUnitTestSources
compile-debug : force
	gradle --daemon compileDebugSources

# We don't have any AndroidTests (yet)
test : force
	gradle --daemon compileDebugSources compileDebugUnitTestSources

clean : test-clean
	rm -rf build/*
	rm -rf app/build/*

# when run from AndroidStudio, process dir is ./app; when run from Makefile, it's .
test-clean :
	rm -rf app/tmp tmp

source-clean :
	find .. -name "*~" -print -delete

tag :
	mtn tag h:org.stephe_leake.music_player.java version-$(VERSION)

archive :
	cp build/outputs/apk/org.stephe_leake.music_player.java-release-$(VERSION).apk /cygdrive/d/Archive/Android

install-emulator-all : compile install-emulator-debug

# FIXME: fails with INSTALL_PARSE_FAILED_INCONSISTENT_CERTIFICATES
# but works from Android Studio (which also provides log filters). and debug works.
install-emulator-release :
	adb -e install -r build/outputs/apk/org.stephe_leake.music_player.java-release-$(VERSION).apk

install-emulator-debug :
	adb -e install -r build/outputs/apk/org.stephe_leake.music_player.java-debug-$(VERSION).apk

install-pda-usb-release :
	adb -d install -r build/outputs/apk/org.stephe_leake.music_player.java-release-$(VERSION).apk

install-pda-usb-debug-all : compile-debug install-pda-usb-debug
install-pda-usb-debug :
	adb -d install -r app/build/outputs/apk/app-debug-$(VERSION).apk

# assumes PDA is running sshDroid server, ssh key is active
# apk version is in build.gradle
install-pda-ssh-release-all : all install-pda-ssh-release

install-pda-ssh-release : PDA_IP := 192.168.1.66
install-pda-ssh-release :
	scp -P 2222 build/outputs/apk/org.stephe_leake.music_player.java-release-$(VERSION).apk root@$(PDA_IP):/storage/sdcard0/Download/

install-pda-ssh-debug-all : all install-pda-ssh-debug
install-pda-ssh-debug : PDA_IP := 192.168.1.66
install-pda-ssh-debug :
	scp -P 2222 build/outputs/apk/org.stephe_leake.music_player.java-debug-$(VERSION).apk root@$(PDA_IP):/storage/sdcard0/Download/

# only needed once in newly created virtual device; image is persistant
install-playlist :
	adb -e push test/Audio/vocal.m3u /storage/sdcard/Audio/vocal.m3u
	adb -e push "test/Audio/vocal/David Wilcox/Into the Mystery/12 Native Tongue.mp3" "/storage/sdcard/Audio/vocal/David Wilcox/Into the Mystery/"
	adb -e push "test/Audio/vocal/David Wilcox/Into the Mystery/AlbumArt_Large.jpg" "/storage/sdcard/Audio/vocal/David Wilcox/Into the Mystery/"
	adb -e push "test/Audio/vocal/David Wilcox/Into the Mystery/liner_notes.pdf" "/storage/sdcard/Audio/vocal/David Wilcox/Into the Mystery/"
	adb -e push "test/Audio/vocal/Joni Mitchell/Don Juan's Reckless Daughter/Paprika Plains.mp3" "/storage/sdcard/Audio/vocal/Joni Mitchell/Don Juan's Reckless Daughter/Paprika Plains.mp3"
	adb -e push "test/Audio/vocal/Joni Mitchell/Don Juan's Reckless Daughter/AlbumArt_Huge.jpg" "/storage/sdcard/Audio/vocal/Joni Mitchell/Don Juan's Reckless Daughter/AlbumArt_Huge.jpg"
	adb -e push "test/Audio/vocal/Mary Chapin Carpenter/State of the Heart/Down in Mary's Land.mp3" "/storage/sdcard/Audio/vocal/Mary Chapin Carpenter/State of the Heart/Down in Mary's Land.mp3"
	adb -e push "test/Audio/vocal/Mary Chapin Carpenter/State of the Heart/AlbumArt_Large.jpg" "/storage/sdcard/Audio/vocal/Mary Chapin Carpenter/State of the Heart/AlbumArt_Large.jpg"

# In the Galaxy Tab emulator, /media is not world write, so change mode to o+rw here.
# FIXME: have to redo 'chmod 777 /data' on each emulator boot.
install-playlist-2 :
	adb -e chmod 777 /data
	adb -e chmod 777 /data/media
	adb -e push test/Audio/vocal.m3u /data/media/Audio/vocal.m3u
	adb -e push "d:/Music/David Wilcox/Into the Mystery/12 Native Tongue.mp3" "/data/media/Audio/vocal/David Wilcox/Into the Mystery/12 Native Tongue.mp3"
	adb -e push "d:/Music/David Wilcox/Into the Mystery/AlbumArt_{34040FCC-77F8-4958-80DF-758892F2BF8F}_Large.jpg" "/data/media/Audio/vocal/David Wilcox/Into the Mystery/AlbumArt_{34040FCC-77F8-4958-80DF-758892F2BF8F}_Large.jpg"
	adb -e push "d:/Music/Joni Mitchell/Don Juan's Reckless Daughter/Paprika Plains.mp3" "/data/media/Audio/vocal/Joni Mitchell/Don Juan's Reckless Daughter/Paprika Plains.mp3"
	adb -e push "d:/Music/Mary Chapin Carpenter/State of the Heart/Down in Mary's Land.mp3" "/data/media/Audio/vocal/Mary Chapin Carpenter/State of the Heart/Down in Mary's Land.mp3"

# Local Variables:
# eval: (load (expand-file-name "prj.el"))
# End:

# end of file
