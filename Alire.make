# Build smm with Alire

#default is debug
#ALIRE_BUILD_ARGS :? --release

STEPHES_ADA_LIBRARY_ALIRE_PREFIX ?= $(CURDIR)/../org.stephe_leake.sal

include $(STEPHES_ADA_LIBRARY_ALIRE_PREFIX)/build/alire_rules.make

# if 'all' target fails due to alire stuff, use 'alire-build'. otherwise, this is faster.

all : alr.env force
	source ./alr.env; gprbuild -P build/smm_alire.gpr

obj/development/smm.exe : alr.env force
	. ./alr.env; gprbuild -P build/smm_alire.gpr smm-driver.adb

clean : alire-clean
	rm -f alr.env

# this also cleans dependencies
really-clean : clean
	rm -rf ~/.config/alire/cache/builds

t1 : alire-build
	build/obj/development/smm.exe compare_best

.PHONEY : t1

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-alire.el"))
# End:
# end of file
