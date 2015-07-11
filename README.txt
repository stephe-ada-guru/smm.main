This is Stephe's Music Player.

Copyright 2011-2015 Stephen Leake stephen_leake@stephe-leake.org

Released under GPLv3+ (see COPYING3) or Apache version 2.0 (see
http://www.apache.org/licenses/LICENSE-2.0)

See design.text for overview of app design.

Build tools:

cygwin make, bash etc
    to run everything
    http://www.cygwin.com

java
    to run android SDK tools, ant, Android Studio (64 bit)

    http://www.oracle.com/technetwork/java/
        java SE 8u45, JDK, Windows 64 bit

    install to default location
        if change it, Android Studio can't find it!
            more precisely, Android Studio captures the path to the JDK on first startup
            so if you move it later, Android Studio can't find it
            and there's no way to get back to the initial startup screen short of reinstalling Android Studio

        not clear how to install both 32 and 64 bit versions.
        don't need JavaFX

    add to ~/develop_settings.el exec-path:
        c:/Program Files (x86)/Java/jdk1.8.0_45/bin

android Studio and SDK
    install Android Studio to create new projects, see what it does, and to get 64 bit SDK, gradle

    Android 4, platform 19 for Galaxy Note 3

    http://developer.android.com/sdk/index.html
        Android Studio, version 24.3.3 June 2015; Android platform 19 and later

    install java first
    install Android Studio to default location

    ~/develop_settings.el sal-standard
        ANDROID_HOME c:/Apps/android-sdk-23.3.3

        exec-path:
            $ANDROID_HOME/tools
            $ANDROID_HOME/platform-tools

    run SDK manager
        download Android target(s)
            4.4.2 API 19, ARM for Galaxy Note III (Qualcomm Snapdragon Krait CPU)

            need Tools, platform-tools, build-tools (all different versions)
            need Android <version>
            don't install Google USB driver (unless want to try debugger again)

    copy :/Apps/android-sdk-24.3.3/extras/android/support/v4/android-support-v4.jar to ./libs
        to include in apk

ant
    android SDK assumes this build tool (even though make is better :)
    http://ant.apache.org/ 1.9.6
    http://ant.apache.org/manual/install.html#installing
        untar to /Apps/apache-ant-*

    in ~/develop_settings.el sal-standard:
        ANT_HOME /Apps/apache-ant-*
        exec-path: $ANT_HOME/bin

edit AndroidManifest.xml android:minSdkVersion match API level

build/Makefile
    make -r all

 When all else fails:

When the Android SDK changes, follow the Android tutorial for creating
a new project using command line tools
(http://developer.android.com/tools/projects/projects-cmdline.html),
compare build.xml. On Windows, you'll have to use cmd.exe, because the
SDK installs only a DOS android.bat, not the bash android script.

\Apps\android-sdk-24.3.3\tools\android.bat create project --activity foo_act --package foo.my_package --target android-19 --path foo_path
