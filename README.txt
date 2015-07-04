This is Stephe's Music Player.

Copyright 2011-2015 Stephen Leake stephen_leake@stephe-leake.org

Released under GPLv3+ (see COPYING3) or Apache version 2.0 (see
http://www.apache.org/licenses/LICENSE-2.0)

Build tools:

cygwin make, bash etc
    to run everything
    http://www.cygwin.com

java
    to run android SDK (32 bit!), ant
        need 32 bit, not 64 bit, for Android SDK to see it

    http://www.oracle.com/technetwork/java/
        java SE 8u45, JDK, Windows 32 bit

    install to default location
        don't need JavaFX

    add to ~/develop_settings.el exec-path:
        c:/Program Files (x86)/Java/jdk1.8.0_45/bin

android SDK
    Android 4, platform 19 for Galaxy Note 3
    all SDK tools are 32 bit

    http://developer.android.com/sdk/index.html
        SDK tools only, version 24.3.3 June 2015; Android platform 19 and later

    install java first
    install to /Apps/android-sdk-24.3.3

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
