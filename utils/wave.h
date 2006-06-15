#ifndef SOUND_WAVE_H
#define SOUND_WAVE_H


namespace Common { class SeekableReadStream; }

namespace Audio {

class AudioStream;

/**
 * Try to load a WAVE from the given seekable stream. Returns true if
 * successful. In that case, the stream's seek position will be set to the
 * start of the audio data, and size, rate and flags contain information
 * necessary for playback. Currently this function only supports uncompressed
 * raw PCM data as well as IMA ADPCM.
 */
extern bool loadWAVFromStream(Common::SeekableReadStream &stream, int &size, int &rate, byte &flags, uint16 *wavType = 0, int *blockAlign = 0);

/**
 * Try to load a WAVE from the given seekable stream and create an AudioStream
 * from that data. Currently this function only supports uncompressed raw PCM
 * data as well as IMA ADPCM.
 *
 * This function uses loadWAVFromStream() internally.
 */
AudioStream *makeWAVStream(Common::SeekableReadStream &stream);

} // End of namespace Audio

#endif
