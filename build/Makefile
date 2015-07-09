# top level Makefile to build Stephe's Music Player app

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
	mtn tag h:org.stephe_leake.music_player.java version-4.2

# assumes PDA is running sshDroid server, ssh key is active
# apk version is in ../app/build.gradle
install-pda-ssh-all : all install-pda-ssh

install-pda-ssh : PDA_IP := 192.168.1.66
install-pda-ssh : VERSION := 5
install-pda-ssh :
	scp -P 2222 ../app/build/outputs/apk/app-release-$(VERSION).apk root@$(PDA_IP):/storage/sdcard0/Download/org.stephe_leake.android.stephes_music.$(VERSION)-release.apk

# see jde project file ../prj.el
# end of file
