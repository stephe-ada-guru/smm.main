# top level Makefile to build Stephe's Music Player Ada app

# These must match ../AndroidManifest.xml
#
# package
ANDROID_PROJECT_PATH := org/stephe_leake/android/stephes_music
#
# activity name
ANDROID_PROJECT_NAME := activity

ANDROID_PROJECT_SRC := service
ANDROID_PROJECT_SRC := utils

ANDROID_PROJECT_RESOURCES += ../res/layout/main.xml
ANDROID_PROJECT_RESOURCES += ../res/values/strings.xml

SDK := /Apps/Android/android-sdk/platforms/android-10

include android.make

all : $(APK_NAME)

## test targets

install-emulator: ANDROID_DEVICE := -e
install-emulator: install

# push music files to running android emulator
sdcard :
	adb shell echo "Vocal/01 - Fill Me Up.mp3" > /sdcard/Audio/vocal.m3u
	adb shell echo "Vocal/02 - These Four Walls.mp3" >> /sdcard/Audio/vocal.m3u
	adb push "/Projects/Music/Shawn Colvin/These Four Walls/01 - Fill Me Up.mp3" /sdcard/Audio/Vocal/
	adb push "/Projects/Music/Shawn Colvin/These Four Walls/02 - These Four Walls.mp3" /sdcard/Audio/Vocal/

app-intent-main :
	adb shell am start -n org.stephe_leake.android.music_player/.Activity -a android.intent.action.MAIN -c android.intent.category.LAUNCHER

app-intent-view :
	adb shell am start -n org.stephe_leake.android.music_player/.Activity -a android.intent.action.VIEW -c android.intent.category.DEFAULT  -t "audio/x-mpegurl" -d "file:///mnt/sdcard/Audio/vocal.m3u"

#Local Variables:
#eval: (setenv "ADA_PROJECT_PATH" "/Projects/android/android-10-ada")
#eval: (ada-parse-prj-file "app.gpr")
#eval: (ada-select-prj-file "app.gpr")
#eval: (load "c:/Projects/android/gnat/gnatvm-2011-src/fix-limited-with.el")
#eval: (makefile-mode)
#End:
# end of file
