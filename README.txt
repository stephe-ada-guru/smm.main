This is Stephe's Music Player.

Copyright 2011 Stephen Leake stephen_leake@stephe-leake.org

Released under GPLv3+ (see COPYING3) or Apache version 2.0 (see
http://www.apache.org/licenses/LICENSE-2.0)

Build tools:

run all installers as admin

java
    to run android SDK, ant
    http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1637583.html
    http://download.oracle.com/otn-pub/java/jdk/7u5-b06/jdk-7u5-windows-i586.exe
        need 32 bit, not 64 bit, for Cygwin and Android SDK to see it

    install to default location
        don't need JavaFX

    add to PATH:
        c:/Program Files (x86)/Java/jdk1.7.0_05/bin

    edit build/Makefile JAVA_TARGET to match installed version

android SDK
    to compile the app
    http://developer.android.com/sdk/index.html
        installer_r20.0.1-windows.exe

    install java first
    install to /Apps/android-sdk-20.0.1

    run SDK manager
        download your Android target(s)
            2.3.6 for Galaxy Note (may update)
            2.3.3 for HTC Inspire

    set ANDROID_HOME to c:/Apps/android-sdk-20.0.1

    add to PATH:
        /Apps/android-sdk-20.0.1/tools
        /Apps/android-sdk-20.0.1/platform-tools

cygwin make, bash etc
    to run everything
    http://www.cygwin.com

ant
    android SDK assumes this build tool (even though make is better :)
    http://ant.apache.org/
        apache-ant-1.8.4-bin.tar.gz
    http://ant.apache.org/manual/install.html#installing
        untar to /Apps/apache-ant-1.8.4
        set ANT_HOME to /Apps/apache-ant-1.8.4/
        add $ANT_HOME/bin to PATH

edit local.properties sdk.dir to match android-sdk location, version

make -C build all

When the Android SDK changes, follow the Android tutorial for creating
a new project using command line tools, compare build.xml. On Windows,
you'll have to use cmd.exe, because the SDK installs only a DOS
android.bat, not the bash android script.
