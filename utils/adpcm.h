#ifndef SOUND_ADPCM_H
#define SOUND_ADPCM_H

#include "audiostream.h"
#include "stream.h"

namespace Audio {

class AudioStream;

enum typesADPCM {
	kADPCMOki,
	kADPCMMSIma,
	kADPCMMS
};

AudioStream *makeADPCMStream(Common::SeekableReadStream *stream, uint32 size, typesADPCM type, int rate = 22050, int channels = 2, uint32 blockAlign = 0);

} // End of namespace Audio

#endif
