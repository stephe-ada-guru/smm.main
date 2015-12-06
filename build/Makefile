# top level Makefile to build Stephe's Music Player app

# must match ../app/build.gradle versionName, versionCode
VERSION := 8

.PHONY : force

all : compile

#  --info gives more detail
# gradle lint output in:
# (browse-url "../app/build/outputs/lint-results-release-fatal.html")
# (browse-url "../app/build/outputs/lint-results.html")
compile : force
	cd ..; gradle build

clean :
	rm -rf intermediates ../app/build/*

source-clean :
	find .. -name "*~" -print -delete

tag :
	mtn tag h:org.stephe_leake.music_player.java version-$(VERSION)

install-emulator-all : all install-emulator

# FIXME: fails with INSTALL_PARSE_FAILED_INCONSISTENT_CERTIFICATES
# but works from Android Studio (which also provides log filters)
install-emulator :
	adb -e install -r ../app/build/outputs/apk/app-release-$(VERSION).apk

# assumes PDA is running sshDroid server, ssh key is active
# apk version is in ../app/build.gradle
install-pda-ssh-release-all : all install-pda-ssh-release

install-pda-ssh-release : PDA_IP := 192.168.1.66
install-pda-ssh-release :
	scp -P 2222 ../app/build/outputs/apk/app-release-$(VERSION).apk root@$(PDA_IP):/storage/sdcard0/Download/org.stephe_leake.android.stephes_music.$(VERSION)-release.apk

install-pda-ssh-debug-all : all install-pda-ssh-debug
install-pda-ssh-debug : PDA_IP := 192.168.1.66
install-pda-ssh-debug :
	scp -P 2222 ../app/build/outputs/apk/app-debug-$(VERSION).apk root@$(PDA_IP):/storage/sdcard0/Download/org.stephe_leake.android.stephes_music.$(VERSION)-debug.apk

# only needed once in newly created virtual device; image is persistant
install-playlist :
	adb -e push ../test/Audio/vocal.m3u /storage/sdcard/Audio/vocal.m3u
	adb -e push "../test/Audio/vocal/David Wilcox/Into the Mystery/12 Native Tongue.mp3" "/storage/sdcard/Audio/vocal/David Wilcox/Into the Mystery/12 Native Tongue.mp3"
	adb -e push "../test/Audio/vocal/David Wilcox/Into the Mystery/AlbumArt_{34040FCC-77F8-4958-80DF-758892F2BF8F}_Large.jpg" "/storage/sdcard/Audio/vocal/David Wilcox/Into the Mystery/AlbumArt_{34040FCC-77F8-4958-80DF-758892F2BF8F}_Large.jpg"
	adb -e push "../test/Audio/vocal/Joni Mitchell/Don Juan's Reckless Daughter/Paprika Plains.mp3" "/storage/sdcard/Audio/vocal/Joni Mitchell/Don Juan's Reckless Daughter/Paprika Plains.mp3"
	adb -e push "../test/Audio/vocal/Mary Chapin Carpenter/State of the Heart/Down in Mary's Land.mp3" "/storage/sdcard/Audio/vocal/Mary Chapin Carpenter/State of the Heart/Down in Mary's Land.mp3"


# Local Variables:
# eval: (load (expand-file-name "../prj.el"))
# End:

# end of file
