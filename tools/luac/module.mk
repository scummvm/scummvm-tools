MODULE := tools/luac

TOOL_OBJS := \
	dump.o \
	luac.o \
	opcode.o \
	opt.o \
	print.o \
	rebase.o

TOOL_LDFLAGS := tools/lua/liblua.a

TOOL := luac
TOOL_DEPS := tools/lua

MAKE := luac

# Include common rules
include $(srcdir)/rules.mk

