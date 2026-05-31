# used by Makefile

vpath %.adb source test
vpath %.svg source

include ../../org.stephe_leake.makerules/common_rules.make

# don't strip, so stack traceback is useful on errors
$(HOME)/.local/bin/%.exe : bin/%.exe
	cp $^ $@

$(SERVER_DATA)/liner_notes_icon-desktop.png $(SERVER_DATA)/liner_notes_icon-tablet.png $(SERVER_DATA)/liner_notes_icon-phone.png : liner_notes_icon.svg
	rsvg-convert -h 50 -a $< > $@

$(SERVER_DATA)/play_icon-desktop.png $(SERVER_DATA)/play_icon-tablet.png $(SERVER_DATA)/play_icon-phone.png : play_icon.svg
	rsvg-convert -h 10 -a $< > $@

$(SERVER_DATA)/app_icon.png : app_icon.svg
	rsvg-convert -h 20 -a $< > $@

$(SERVER_DATA)/% : source/%
	cp $^ $@

test_%.exe :
	alr exec -- gprbuild -P smm_test.gpr $@.adb

bin/%.exe : force
	alr exec -- gprbuild -P smm.gpr $(*F).adb

create_test_db :
	mkdir -p tmp/source
	echo "Root = " $(CURDIR) "/tmp/source" > tmp/smm.db

sqlite-clean :
	rm -f smm*.db
	rm -f tmp/smm*.db

clean :: sqlite-clean
	rm -fr tmp

# end of file
