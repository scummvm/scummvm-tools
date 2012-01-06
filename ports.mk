# This file contains port specific Makefile rules. It is automatically
# included by the default (main) Makefile.
#

#
# UNIX specific
#
install: all
	$(INSTALL) -d "$(DESTDIR)$(BINDIR)"
	$(INSTALL) -c -s -m 755 "./$(EXECUTABLE)" "$(DESTDIR)$(BINDIR)/$(EXECUTABLE)"
	$(INSTALL) -d "$(DESTDIR)$(PREFIX)/share/doc/residualvm-tools/"
	$(INSTALL) -c -m 644 $(DIST_FILES_DOCS) "$(DESTDIR)$(PREFIX)/share/doc/residualvm-tools/"
	$(INSTALL) -d "$(DESTDIR)$(DATADIR)/residualvm-tools/"
	$(INSTALL) -c -m 644 $(DIST_FILES_THEMES) "$(DESTDIR)$(DATADIR)/residualvm-tools/"
	#$(INSTALL) -c -m 644 $(DIST_FILES_ENGINEDATA) "$(DESTDIR)$(DATADIR)/residualvm-tools/"

uninstall:
	rm -f "$(DESTDIR)$(BINDIR)/$(EXECUTABLE)"
	rm -rf "$(DESTDIR)$(PREFIX)/share/doc/residualvm-tools/"
	rm -rf "$(DESTDIR)$(DATADIR)/residualvm-tools/"


#
# ARM specific
#
ifdef USE_TREMOLO
DEFINES += -DUSE_TREMOR -DUSE_VORBIS -DUSE_TREMOLO
LIBS += -ltremolo
endif

