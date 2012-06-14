# top level Makefile to build Stephe's Music Player app

# These must match ../AndroidManifest.xml
#
# package
ANDROID_PROJECT_PATH := org/stephe_leake/android/stephes_music
#
# activity name
ANDROID_PROJECT_NAME := activity

ANDROID_PROJECT_RESOURCES += ../res/layout-normal/main.xml
ANDROID_PROJECT_RESOURCES += ../res/layout-large-land/main.xml
ANDROID_PROJECT_RESOURCES += ../res/layout-large-port/main.xml
ANDROID_PROJECT_RESOURCES += ../res/values/strings.xml
ANDROID_PROJECT_RESOURCES += ../res/xml/preferences.xml

# all other source files for make dependencies
ANDROID_PROJECT_SRC += preferences
ANDROID_PROJECT_SRC += service
ANDROID_PROJECT_SRC += utils

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
