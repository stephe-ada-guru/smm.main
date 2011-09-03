# misc tasks; use Eclipse for most stuff

copy:
	cp bin/Stephes_Music_PlayerActivity.apk e:/download

# restart (or just close :) Eclipse if switch from emulator to HTC
install:
	adb -d install -r bin/Stephes_Music_PlayerActivity.apk

# start an emulator
emu :
	emulator -avd HTC_Inspire_2.3

# erase cached sdcard image
erase-sdcard :
	emulator -avd HTC_Inspire_2.3 -wipe-data &

# push music files to running android emulator
sdcard :
	adb shell echo "Vocal/01 - Fill Me Up.mp3" > /sdcard/Audio/vocal.m3u
	adb shell echo "Vocal/02 - These Four Walls.mp3" >> /sdcard/Audio/vocal.m3u
	adb push "/Projects/Music/Shawn Colvin/These Four Walls/01 - Fill Me Up.mp3" /sdcard/Audio/Vocal/
	adb push "/Projects/Music/Shawn Colvin/These Four Walls/02 - These Four Walls.mp3" /sdcard/Audio/Vocal/

applog :
	adb shell dumpsys activity service Stephes_Music_Service

syslog :
	adb shell logcat -d

app-intent-main :
	adb shell am start -n org.stephe_leake.android.music_player/.Stephes_Music_PlayerActivity -a android.intent.action.MAIN -c android.intent.category.LAUNCHER

app-intent-view :
	adb shell am start -n org.stephe_leake.android.music_player/.Stephes_Music_PlayerActivity -a android.intent.action.VIEW -c android.intent.category.DEFAULT  -t "audio/x-mpegurl" -d "file:///mnt/sdcard/Audio/vocal.m3u"

# if MediaButtonIntentReceiver gets
# AudioManager.ACTION_AUDIO_BECOMING_NOISY, it starts
# Stephes_Music_Service with intent SERVICECMD CMDPAUSE; test the
# latter (need a test driver app to test the former?)
service-intent-audio_noisy :
	am startservice -n  org.stephe_leake.android.music_player/.Stephes_Music_Service -a "org.stephe_leake.android.music_player.musicservicecommand" -e "command" "pause"

# end of file
