# top level Makefile to build Stephe's Music Player app

.PHONY : force

# lint looks for compiled classes, to it has to run after the compiler
all : compile lint

# c:/Apps/android-*/ sdk tools requires 32 bit java
# (java-32)
compile : force
	cd ..; ant release

lint :
	cmd.exe /c lint ..

clean :
	rm -rf ../bin/* ../gen/*

source-clean :
	find .. -name "*~" -print -delete

tag :
	mtn tag h:org.stephe_leake.music_player.java version-4.2

# assumes PDA is running sshDroid server, ssh key is active
PDA_IP := 192.168.1.66
install-pda-ssh-all : all install-pda-ssh
install-pda-ssh :
	scp -P 2222 ../bin/org.stephe_leake.android.stephes_music.4.3-release.apk root@$(PDA_IP):/storage/sdcard0/Download

# see jde project file ../prj.el
# end of file
