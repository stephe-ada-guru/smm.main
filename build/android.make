# Make rules for building Android apps in Java, using javac and Android SDK tools

# paths relative to package/build
vpath %.java  ../src/$(ANDROID_PROJECT_PATH)
vpath %.apk   ../bin
vpath %.class ../bin/classes

APK_NAME := $(subst /,.,$(ANDROID_PROJECT_PATH)).$(ANDROID_PROJECT_NAME)-release.apk

ANDROID_PROJECT_CLASS := $(addsuffix .class,$(ANDROID_PROJECT_NAME) $(ANDROID_PROJECT_SRC))

$(APK_NAME) : $(ANDROID_PROJECT_CLASS)
$(APK_NAME) : ../bin/classes/$(ANDROID_PROJECT_PATH)/R.class
	cd ..; ant release

../bin/classes/$(ANDROID_PROJECT_PATH)/R.class : ../gen/$(ANDROID_PROJECT_PATH)/R.java
	javac -target 1.5 -d ../bin/classes $^

# FIXME: use aapt directly?
../gen/$(ANDROID_PROJECT_PATH)/R.java : $(ANDROID_PROJECT_RESOURCES)
	cd ..; ant resource-src

# don't depend on $(APK_NAME) here, so it is not rebuilt each time.
# install to $(ANDROID_DEVICE) = { | -d | -e }
install :
	cd ../bin/; adb $(ANDROID_DEVICE) install -r $(APK_NAME)

.PHONY : force

ANDROID_JAR := $(ANDROID_SDK)/platforms/android-10/android.jar
%.class : %.java | ../bin/classes
	javac -g -deprecation -cp $(ANDROID_JAR) -sourcepath "../src;../gen" -d ../bin/classes -target 1.5 $<

../bin/classes :
	mkdir -p ../bin/classes

clean :
	rm -rf ../bin/* ../gen/*

source-clean :
	find .. -name "*~" -print -delete

# end of file
