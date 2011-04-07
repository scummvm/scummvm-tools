# This file contains port specific Makefile rules. It is automatically
# included by the default (main) Makefile.
#
# $URL: https://residual.svn.sourceforge.net/svnroot/residual/residual/trunk/ports.mk $
# $Id: ports.mk 1635 2010-01-21 19:25:03Z aquadran $


#
# UNIX specific
#
install: all
	$(INSTALL) -d "$(DESTDIR)$(BINDIR)"
	$(INSTALL) -c -s -m 755 "./$(EXECUTABLE)" "$(DESTDIR)$(BINDIR)/$(EXECUTABLE)"
	$(INSTALL) -d "$(DESTDIR)$(PREFIX)/share/doc/residual-tools/"
	$(INSTALL) -c -m 644 $(DIST_FILES_DOCS) "$(DESTDIR)$(PREFIX)/share/doc/residual-tools/"
	$(INSTALL) -d "$(DESTDIR)$(DATADIR)/residual-tools/"
	$(INSTALL) -c -m 644 $(DIST_FILES_THEMES) "$(DESTDIR)$(DATADIR)/residual-tools/"
	#$(INSTALL) -c -m 644 $(DIST_FILES_ENGINEDATA) "$(DESTDIR)$(DATADIR)/residual-tools/"

uninstall:
	rm -f "$(DESTDIR)$(BINDIR)/$(EXECUTABLE)"
	rm -rf "$(DESTDIR)$(PREFIX)/share/doc/residual-tools/"
	rm -rf "$(DESTDIR)$(DATADIR)/residual-tools/"


#
# ARM specific
#
ifdef USE_TREMOLO
DEFINES += -DUSE_TREMOR -DUSE_VORBIS -DUSE_TREMOLO
LIBS += -ltremolo
endif

