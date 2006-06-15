#ifndef COMMON_MD5_H
#define COMMON_MD5_H

#include "../util.h"

namespace Common {

typedef struct {
	uint32 total[2];
	uint32 state[4];
	uint8 buffer[64];
} md5_context;

void md5_starts(md5_context *ctx);
void md5_update(md5_context *ctx, const uint8 *input, uint32 length);
void md5_finish(md5_context *ctx, uint8 digest[16]);

bool md5_file(const char *name, uint8 digest[16], uint32 length = 0);

} // End of namespace Common

#endif
