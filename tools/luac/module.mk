MODULE := tools/luac

TOOL_OBJS := \
	dump.o \
	luac.o \
	opcode.o \
	opt.o \
	print.o \
	rebase.o \

TOOL := luac
TOOL_DEPS := tools/lua
TOOL_LDFLAGS := -Ltools/lua -llua

MAKE := luac

# Include common rules
include $(srcdir)/rules.mk

