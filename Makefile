# top level Makefile to build Stephe's Music Player app

# must match build.gradle versionName, versionCode
VERSION := 15

.PHONY : force

all : compile

vpath %.java   app/src/test/java/

CLASS_PATH := app/build/intermediates/classes/release/
CLASS_PATH := $(CLASS_PATH);app/build/intermediates/classes/test/
CLASS_PATH := $(CLASS_PATH);d:/Archive/Android/libs/junit-4.12.jar
CLASS_PATH := $(CLASS_PATH);d:/Archive/Android/libs/hamcrest-core-1.3.jar
CLASS_PATH := $(CLASS_PATH);d:/Archive/Android/libs/commons-io-2.5.jar

TEST_CLASSES := $(CLASS_DIR)org/stephe_leake/android/stephes_music/TestSyncUtils.class

$(CLASS_DIR)%.class : %.java
	javac -cp "$(CLASS_PATH)" -d app/build/intermediates/classes/test/ $^

test-host : $(TEST_CLASSES)
	java -cp "$(CLASS_PATH)" org.junit.runner.JUnitCore org.stephe_leake.android.stephes_music.TestSyncUtils


#  --info gives more detail, --stacktrace gives error stack trace
# gradle lint output in:
# (browse-url "build/outputs/lint-results-release-fatal.html")
# (browse-url "build/outputs/lint-results.html")
compile : force
	gradle build

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

install-emulator-all : all install-emulator-debug

# FIXME: fails with INSTALL_PARSE_FAILED_INCONSISTENT_CERTIFICATES
# but works from Android Studio (which also provides log filters). and debug works.
install-emulator-release :
	adb -e install -r build/outputs/apk/org.stephe_leake.music_player.java-release-$(VERSION).apk

install-emulator-debug :
	adb -e install -r build/outputs/apk/org.stephe_leake.music_player.java-debug-$(VERSION).apk

install-pda-usb-release :
	adb -d install -r build/outputs/apk/org.stephe_leake.music_player.java-release-$(VERSION).apk

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
