######################################################################
# Unit/regression tests, based on CxxTest.
# Use the 'test' target to run them.
# Edit TESTS and TESTLIBS to add more tests.
#
######################################################################

TESTS        := $(srcdir)/decompiler/test/*.h
TEST_LIBS    := \
	common/file.o\
	decompiler/disassembler.o \
	decompiler/simple_disassembler.o \
	decompiler/test/disassembler/pasc.o \
	decompiler/test/disassembler/subopcode.o	\
	decompiler/unknown_opcode.o \

#
TEST_FLAGS   := --runner=StdioPrinter
TEST_CFLAGS  := -I$(srcdir)/decompiler/test/cxxtest
TEST_LDFLAGS := $(decompile_LIBS) $(LDFLAGS)

ifdef HAVE_GCC3
# In test/common/str.h, we test a zero length format string. This causes GCC
# to generate a warning which in turn poses a problem when building with -Werror.
# To work around this, we disable -Wformat here.
TEST_CFLAGS  +=  -Wno-format
endif

# Enable this to get an X11 GUI for the error reporter.
#TEST_FLAGS   += --gui=X11Gui
#TEST_LDFLAGS += -L/usr/X11R6/lib -lX11


test: decompiler/test/runner
	./decompiler/test/runner
decompiler/test/runner: decompiler/test/runner.cpp $(TEST_LIBS)
	$(QUIET_LINK)$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(TEST_LDFLAGS) $(TEST_CFLAGS) -o $@ $+
decompiler/test/runner.cpp: $(TESTS)
	@mkdir -p decompiler
	@mkdir -p decompiler/test
	python $(srcdir)/decompiler/test/cxxtest/cxxtestgen.py $(TEST_FLAGS) -o $@ $+


clean: clean-test
clean-test:
	-$(RM) decompiler/test/runner.cpp decompiler/test/runner

.PHONY: test clean-test
