# Build smm with Alire

#default is debug
#ALIRE_BUILD_ARGS :? --release

STEPHES_ADA_LIBRARY_ALIRE_PREFIX ?= $(CURDIR)/../org.stephe_leake.sal

include $(STEPHES_ADA_LIBRARY_ALIRE_PREFIX)/build/alire_rules.make

all : alr.env force
	source ./alr.env; gprbuild -P build/smm_alire.gpr

install : force
	$(MAKE) -c build install

obj/development/%.exe : alr.env force
	. ./alr.env; gprbuild -P build/smm_alire.gpr $(*F)

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-alire.el"))
# End:
# end of file
