MODULE := common

MODULE_OBJS := \
	debug.o \
	file.o \
	hashmap.o \
	md5.o \
	memorypool.o \
	str.o \
	textconsole.o \
	util.o \
	zlib.o

# Include common rules
include $(srcdir)/rules.mk
