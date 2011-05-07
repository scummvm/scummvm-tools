# $URL: https://residual.svn.sourceforge.net/svnroot/residual/residual/trunk/tools/module.mk $
# $Id: module.mk 1426 2009-05-31 08:59:56Z aquadran $

MODULE := tools

MODULE_DIRS += \
	tools/

#######################################################################
# Tools directory
#######################################################################

TOOLS := \
	tools/delua$(EXEEXT) \
	tools/imc2wav$(EXEEXT) \
	tools/int2flt$(EXEEXT) \
	tools/set2fig$(EXEEXT) \
	tools/til2bmp$(EXEEXT) \
	tools/unlab$(EXEEXT) \
	tools/vima$(EXEEXT) \
	tools/patchex/patchex$(EXEEXT)

# below not added as it depends for ppm, bpm library
#	tools/mat2ppm$(EXEEXT)
#	tools/bm2ppm$(EXEEXT)

# Make sure the 'all' / 'clean' targets build/clean the tools, too
#all:
clean: clean-tools

# Main target
tools: $(TOOLS)

clean-tools:
	-$(RM) $(TOOLS)
	-$(RM) tools/patchex/*.o
	-$(RM) -r tools/patchex/.deps

#
# Build rules for the tools
#

tools/delua$(EXEEXT): $(srcdir)/tools/delua.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) $(DEFINES) -DHAVE_CONFIG_H -I$(srcdir) -I. -Wall \
	-L$(srcdir)/common -Ltools/lua -o $@ $< -llua

tools/mat2ppm$(EXEEXT): $(srcdir)/tools/mat2ppm.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -lppm -o $@ $<

tools/bmtoppm$(EXEEXT): $(srcdir)/tools/bmtoppm.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -lppm -lpbm -o $@ $<

tools/imc2wav$(EXEEXT): $(srcdir)/tools/imc2wav.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $<

tools/int2flt$(EXEEXT): $(srcdir)/tools/int2flt.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $<

tools/set2fig$(EXEEXT): $(srcdir)/tools/set2fig.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $<

tools/til2bmp$(EXEEXT): $(srcdir)/tools/til2bmp.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -lz -I. -o $@ $<

tools/unlab$(EXEEXT): $(srcdir)/tools/unlab.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $<

tools/vima$(EXEEXT): $(srcdir)/tools/vima.cpp
	$(MKDIR) tools/$(DEPDIR)
	$(CXX) $(CFLAGS) -Wall -o $@ $<

tools/patchex/patchex$(EXEEXT): tools/patchex/patchex.o tools/patchex/mszipd.o tools/patchex/cabd.o
	$(MKDIR) tools/patchex/$(DEPDIR)
	$(CXX) $(CFLAGS) tools/patchex/mszipd.o tools/patchex/cabd.o -Wall -o $@ $<

.PHONY: clean-tools tools
