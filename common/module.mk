MODULE := common

MODULE_OBJS := \
	md5.o \
	zlib.o

# Include common rules
include $(srcdir)/rules.mk
