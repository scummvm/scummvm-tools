/* Scumm Tools
 * Copyright (C) 2003-2006  The ScummVM Team
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *
 */

#include "compress.h"

#include <sstream>

typedef struct  {
	uint32 minBitr;
	uint32 maxBitr;
	bool abr;
	uint32 algqual;
	uint32 vbrqual;
	bool silent;
} lameparams;

typedef struct {
	int nominalBitr;
	int minBitr;
	int maxBitr;
	float quality;
	bool silent;
} oggencparams;

typedef struct {
	int compressionLevel;
	int blocksize;
	bool verify;
	bool silent;
} flaccparams;

typedef struct {
	bool isLittleEndian, isStereo;
	uint8 bitsPerSample;
} rawtype;

lameparams encparms = { minBitrDef, maxBitrDef, false, algqualDef, vbrqualDef, 0 };
oggencparams oggparms = { -1, -1, -1, (float)oggqualDef, 0 };
flaccparams flacparms = { flacCompressDef, flacBlocksizeDef, false, false };
rawtype	rawAudioType = { false, false, 8 };

const char *tempEncoded = TEMP_MP3;

void CompressionTool::setRawAudioType(bool isLittleEndian, bool isStereo, uint8 bitsPerSample) {
	rawAudioType.isLittleEndian = isLittleEndian;
	rawAudioType.isStereo = isStereo;
	rawAudioType.bitsPerSample = bitsPerSample;
}

int getSampleRateFromVOCRate(int vocSR) {
	if (vocSR == 0xa5 || vocSR == 0xa6 || vocSR == 0x83) {
		return 11025;
	} else if (vocSR == 0xd2 || vocSR == 0xd3) {
		return 22050;
	} else {
		int sr = 1000000L / (256L - vocSR);
		/* inexact sampling rates occur e.g. in the kitchen in Monkey
		 * Island, very easy to reach right from the start of the game.
		 * warning("inexact sample rate used: %i (0x%x)", sr, vocSR);
		 */
		return sr;
	}
}

/* map frequency to a valid MP3 sample frequency
 *
 * Robert Hegemann 2000-07-01
 *
 * Copied from lame 3.96.1
 */
static int map2MP3Frequency(int freq) {
    if (freq <=  8000) return  8000;
    if (freq <= 11025) return 11025;
    if (freq <= 12000) return 12000;
    if (freq <= 16000) return 16000;
    if (freq <= 22050) return 22050;
    if (freq <= 24000) return 24000;
    if (freq <= 32000) return 32000;
    if (freq <= 44100) return 44100;

    return 48000;
}

void CompressionTool::encodeAudio(const char *inname, bool rawInput, int rawSamplerate, const char *outname, AudioFormat compmode) {
	bool err = false;
	char fbuf[2048];
	char *tmp = fbuf;

	if (compmode == AUDIO_MP3) {
		tmp += sprintf(tmp, "lame -t ");
		if (rawInput) {
			tmp += sprintf(tmp, "-r ");
			tmp += sprintf(tmp, "--bitwidth %d ", rawAudioType.bitsPerSample);

			if (rawAudioType.isLittleEndian) {
				tmp += sprintf(tmp, "--little-endian ");
			} else {
				tmp += sprintf(tmp, "--big-endian ");
			}

			tmp += sprintf(tmp, (rawAudioType.isStereo ? "-m j " : "-m m "));
			tmp += sprintf(tmp, "-s %d ", rawSamplerate);
		}

		if (encparms.abr)
			tmp += sprintf(tmp, "--abr %d ", encparms.minBitr);
		else
			tmp += sprintf(tmp, "--vbr-new -b %d ", encparms.minBitr);

		/* Explicitly specify a target sample rate, to work around a bug (?)
		* in newer lame versions (>= 3.95) which causes it to malfunction
		* for odd sample rates when in VBR mode. See also bug #934026.
		* We essentially duplicate the old behaviour of lame (found in e.g.
		* version 3.93.1): we round the input sample rate up to the next
		* higher valid MP3 sample rate, with a margin of 3%.
		*/
		if (rawSamplerate != -1) {
			tmp += sprintf(tmp, "--resample %d ", map2MP3Frequency(97 * rawSamplerate / 100));
		}

		if (encparms.silent) {
			tmp += sprintf(tmp, " --silent ");
		}

		tmp += sprintf(tmp, "-q %d ", encparms.algqual);
		tmp += sprintf(tmp, "-V %d ", encparms.vbrqual);
		tmp += sprintf(tmp, "-B %d ", encparms.maxBitr);
		tmp += sprintf(tmp, "\"%s\" \"%s\" ", inname, outname);

		err = spawnSubprocess(fbuf) != 0;

		if (err) {
			char buf[2048];
			sprintf(buf, "Error in MP3 encoder.(check parameters) \nMP3 Encoder Commandline:%s\n", fbuf);
			throw ToolException(buf, err);
		} else {
			return;
		}
	}

#ifdef DISABLE_BUILTIN_VORBIS
		if (compmode == AUDIO_VORBIS) {
			tmp += sprintf(tmp, "oggenc ");
			if (rawInput) {
				tmp += sprintf(tmp, "--raw ");
				tmp += sprintf(tmp, "--raw-chan=%d ", (rawAudioType.isStereo ? 2 : 1));
				tmp += sprintf(tmp, "--raw-bits=%d ", rawAudioType.bitsPerSample);
				tmp += sprintf(tmp, "--raw-rate=%d ", rawSamplerate);
				tmp += sprintf(tmp, "--raw-endianness=%d ", (rawAudioType.isLittleEndian ? 0 : 1));
			}

			if (oggparms.nominalBitr != -1) {
				tmp += sprintf(tmp, "--bitrate=%d ", oggparms.nominalBitr);
			} else {
				tmp += sprintf(tmp, "--quality=%d ", oggparms.quality);
			}

			if (oggparms.minBitr != -1) {
				tmp += sprintf(tmp, "--min-bitrate=%d ", oggparms.minBitr);
			}

			if (oggparms.maxBitr != -1) {
				tmp += sprintf(tmp, "--max-bitrate=%d ", oggparms.maxBitr);
			}

			if (oggparms.silent) {
				tmp += sprintf(tmp, "--quiet ");
			}

			tmp += sprintf(tmp, "--output=\"%s\" ", outname);
			tmp += sprintf(tmp, "\"%s\" ", inname);

			err = spawnSubprocess(fbuf) != 0;

			if (err) {
				char buf[2048];
				sprintf(buf, "Error in Vorbis encoder. (check parameters)\nVorbis Encoder Commandline:%s\n", fbuf);
				throw ToolException(buf, err);
			} else {
				return;
			}
		}
#endif

#ifdef DISABLE_BUILTIN_FLAC
		if (compmode == AUDIO_FLAC) {
			/* --lax is needed to allow 11kHz, we dont need place for meta-tags, and no seektable */
			/* -f is reqired to force override of unremoved temp file. See bug #1294648 */
			tmp += sprintf(tmp, "flac -f --lax --no-padding --no-seektable --no-ogg ");

			if (rawInput) {
				tmp += sprintf(tmp, "--force-raw-format ");
				tmp += sprintf(tmp, "--sign=%s ", ((rawAudioType.bitsPerSample == 8) ? "unsigned" : "signed"));
				tmp += sprintf(tmp, "--channels=%d ", (rawAudioType.isStereo ? 2 : 1));
				tmp += sprintf(tmp, "--bps=%d ", rawAudioType.bitsPerSample);
				tmp += sprintf(tmp, "--sample-rate=%d ", rawSamplerate);
				tmp += sprintf(tmp, "--endian=%s ", (rawAudioType.isLittleEndian ? "little" : "big"));
			}

			if (flacparms.silent) {
				tmp += sprintf(tmp, "--silent ");
			}

			if (flacparms.verify) {
				tmp += sprintf(tmp, "--verify ");
			}

			tmp += sprintf(tmp, "--compression-level-%d ", flacparms.compressionLevel);
			tmp += sprintf(tmp, "-b %d ", flacparms.blocksize);
			tmp += sprintf(tmp, "-o \"%s\" ", outname);
			tmp += sprintf(tmp, "\"%s\" ", inname);

			err = spawnSubprocess(fbuf) != 0;

			if (err) {
				char buf[2048];
				sprintf(buf, "Error in FLAC encoder. (check parameters)\nFLAC Encoder Commandline:%s\n", fbuf);
				throw ToolException(buf, err);
			} else {
				return;
			}
		}
#endif
		if (rawInput) {
			long length;
			char *rawData;

			File inputRaw(inname, "rb");
			length = inputRaw.size();
			rawData = (char *)malloc(length);
			inputRaw.read(rawData, 1, length);

			print(" - length = %ld\n", length);
			print(" - channels = %d\n", (rawAudioType.isStereo ? 2 : 1));
			print(" - sample rate = %d\n", rawSamplerate);
			print(" - compression = %dbits\n", rawAudioType.bitsPerSample);

			encodeRaw(rawData, length, rawSamplerate, outname, compmode);

			free(rawData);
		} else {
			int fmtHeaderSize, length, numChannels, sampleRate, bitsPerSample;
			char *wavData;

			File inputWav(inname, "rb");

			/* Standard PCM fmt header is 16 bits, but at least Simon 1 and 2 use 18 bits */
			inputWav.seek(16, SEEK_SET);
			fmtHeaderSize = inputWav.readUint32LE();

			inputWav.seek(22, SEEK_SET);
			numChannels = inputWav.readUint16LE();
			sampleRate = inputWav.readUint32LE();

			inputWav.seek(34, SEEK_SET);
			bitsPerSample = inputWav.readUint16LE();

			/* The size of the raw audio is after the RIFF chunk (12 bytes), fmt chunk (8 + fmtHeaderSize bytes), and data chunk id (4 bytes) */
			inputWav.seek(24 + fmtHeaderSize, SEEK_SET);
			length = inputWav.readUint32LE();

			wavData = (char *)malloc(length);
			inputWav.read(wavData, 1, length);

			print(" - length = %d\n", length);
			print(" - channels = %d\n", numChannels);
			print(" - sample rate = %d\n", sampleRate);
			print(" - compression = %dbits\n", bitsPerSample);

			setRawAudioType(true, numChannels == 2, (uint8)bitsPerSample);
			encodeRaw(wavData, length, sampleRate, outname, compmode);

			free (wavData);
		}
}

void CompressionTool::encodeRaw(char *rawData, int length, int samplerate, const char *outname, AudioFormat compmode) {
#ifndef DISABLE_BUILTIN_VORBIS
	if (compmode == AUDIO_VORBIS) {
		char outputString[256] = "";
		int numChannels = (rawAudioType.isStereo ? 2 : 1);
		int totalSamples = length / ((rawAudioType.bitsPerSample / 8) * numChannels);
		int samplesLeft = totalSamples;
		int eos = 0;
		int totalBytes = 0;

		vorbis_info vi;
		vorbis_comment vc;
		vorbis_dsp_state vd;
		vorbis_block vb;

		ogg_stream_state os;
		ogg_page og;
		ogg_packet op;

		ogg_packet header;
		ogg_packet header_comm;
		ogg_packet header_code;

		File outputOgg(outname, "wb");

		vorbis_info_init(&vi);

		if (oggparms.nominalBitr > 0) {
			int result = 0;

			/* Input is in kbps, function takes bps */
			result = vorbis_encode_setup_managed(&vi, numChannels, samplerate, (oggparms.maxBitr > 0 ? 1000 * oggparms.maxBitr : -1), (1000 * oggparms.nominalBitr), (oggparms.minBitr > 0 ? 1000 * oggparms.minBitr : -1));

			if (result == OV_EFAULT) {
				vorbis_info_clear(&vi);
				error("Error: Internal Logic Fault");
			} else if ((result == OV_EINVAL) || (result == OV_EIMPL)) {
				vorbis_info_clear(&vi);
				error("Error: Invalid bitrate parameters");
			}

			if (!oggparms.silent) {
				sprintf(outputString, "Encoding to\n         \"%s\"\nat average bitrate %i kbps (", outname, oggparms.nominalBitr);

				if (oggparms.minBitr > 0) {
					sprintf(outputString + strlen(outputString), "min %i kbps, ", oggparms.minBitr);
				} else {
					sprintf(outputString + strlen(outputString), "no min, ");
				}

				if (oggparms.maxBitr > 0) {
					sprintf(outputString + strlen(outputString), "max %i kbps),\nusing full bitrate management engine\nSet optional hard quality restrictions\n", oggparms.maxBitr);
				} else {
					sprintf(outputString + strlen(outputString), "no max),\nusing full bitrate management engine\nSet optional hard quality restrictions\n");
				}
			}
		} else {
			int result = 0;

			/* Quality input is 1 - 10, function takes -0.1 through 1.0 */
			result = vorbis_encode_setup_vbr(&vi, numChannels, samplerate, oggparms.quality * 0.1f);

			if (result == OV_EFAULT) {
				vorbis_info_clear(&vi);
				error("Internal Logic Fault");
			} else if ((result == OV_EINVAL) || (result == OV_EIMPL)) {
				vorbis_info_clear(&vi);
				error("Invalid bitrate parameters");
			}

			if (!oggparms.silent) {
				sprintf(outputString, "Encoding to\n         \"%s\"\nat quality %2.2f", outname, oggparms.quality);
			}

			if ((oggparms.minBitr > 0) || (oggparms.maxBitr > 0)) {
				struct ovectl_ratemanage_arg extraParam;
				vorbis_encode_ctl(&vi, OV_ECTL_RATEMANAGE_GET, &extraParam);

				extraParam.bitrate_hard_min = (oggparms.minBitr > 0 ? (1000 * oggparms.minBitr) : -1);
				extraParam.bitrate_hard_max = (oggparms.maxBitr > 0 ? (1000 * oggparms.maxBitr) : -1);
				extraParam.management_active = 1;

				vorbis_encode_ctl(&vi, OV_ECTL_RATEMANAGE_SET, &extraParam);

				if (!oggparms.silent) {
					sprintf(outputString + strlen(outputString), " using constrained VBR (");

					if (oggparms.minBitr != -1) {
						sprintf(outputString + strlen(outputString), "min %i kbps, ", oggparms.minBitr);
					} else {
						sprintf(outputString + strlen(outputString), "no min, ");
					}

					if (oggparms.maxBitr != -1) {
						sprintf(outputString + strlen(outputString), "max %i kbps)\nSet optional hard quality restrictions\n", oggparms.maxBitr);
					} else {
						sprintf(outputString + strlen(outputString), "no max)\nSet optional hard quality restrictions\n");
					}
				}
			} else {
				sprintf(outputString + strlen(outputString), "\n");
			}
		}

		puts(outputString);

		vorbis_encode_setup_init(&vi);
		vorbis_comment_init(&vc);
		vorbis_analysis_init(&vd, &vi);
		vorbis_block_init(&vd, &vb);
		ogg_stream_init(&os, 0);
		vorbis_analysis_headerout(&vd, &vc, &header, &header_comm, &header_code);

		ogg_stream_packetin(&os, &header);
		ogg_stream_packetin(&os, &header_comm);
		ogg_stream_packetin(&os, &header_code);

		while (!eos) {
			int result = ogg_stream_flush(&os,&og);

			if (result == 0) {
				break;
			}

			outputOgg.write(og.header, 1, og.header_len);
			outputOgg.write(og.body, 1, og.body_len);
		}

		while (!eos) {
			int i, j;
			int numSamples = ((samplesLeft < 2048) ? samplesLeft : 2048);
			float **buffer = vorbis_analysis_buffer(&vd, numSamples);

			/* We must tell the encoder that we have reached the end of the stream */
			if (numSamples == 0) {
				vorbis_analysis_wrote(&vd, 0);
			} else {
				/* Adapted from oggenc 1.1.1 */
				if (rawAudioType.bitsPerSample == 8) {
					unsigned char *rawDataUnsigned = (unsigned char *)rawData;
					for (i = 0; i < numSamples; i++) {
						for (j = 0; j < numChannels; j++) {
							buffer[j][i] = ((int)(rawDataUnsigned[i * numChannels + j]) - 128) / 128.0f;
						}
					}
				} else if (rawAudioType.bitsPerSample == 16) {
					if (rawAudioType.isLittleEndian) {
						for (i = 0; i < numSamples; i++) {
							for (j = 0; j < numChannels; j++) {
								buffer[j][i] = ((rawData[(i * 2 * numChannels) + (2 * j) + 1] << 8) | (rawData[(i * 2 * numChannels) + (2 * j)] & 0xff)) / 32768.0f;
							}
						}
					}
					else {
						for (i = 0; i < numSamples; i++) {
							for (j = 0; j < numChannels; j++) {
								buffer[j][i] = ((rawData[(i * 2 * numChannels) + (2 * j)] << 8) | (rawData[(i * 2 * numChannels) + (2 * j) + 1] & 0xff)) / 32768.0f;
							}
						}
					}
				}

				vorbis_analysis_wrote(&vd, numSamples);
			}

			while (vorbis_analysis_blockout(&vd, &vb) == 1) {
				vorbis_analysis(&vb, NULL);
				vorbis_bitrate_addblock(&vb);

				while (vorbis_bitrate_flushpacket(&vd, &op)) {
					ogg_stream_packetin(&os, &op);

					while (!eos) {
						int result = ogg_stream_pageout(&os, &og);

						if (result == 0) {
							break;
						}

						totalBytes += outputOgg.write(og.header, 1, og.header_len);
						totalBytes += outputOgg.write(og.body, 1, og.body_len);

						if (ogg_page_eos(&og)) {
							eos = 1;
						}
					}
				}
			}

			rawData += 2048 * (rawAudioType.bitsPerSample / 8) * numChannels;
			samplesLeft -= 2048;
		}

		ogg_stream_clear(&os);
		vorbis_block_clear(&vb);
		vorbis_dsp_clear(&vd);
		vorbis_info_clear(&vi);

		if (!oggparms.silent) {
			print("\nDone encoding file \"%s\"\n", outname);
			print("\n\tFile length:  %dm %ds\n", (int)(totalSamples / samplerate / 60), (totalSamples / samplerate % 60));
			print("\tAverage bitrate: %.1f kb/s\n\n", (8.0 * (double)totalBytes / 1000.0) / ((double)totalSamples / (double)samplerate));
		}
	}
#endif

#ifndef DISABLE_BUILTIN_FLAC
	if (compmode == AUDIO_FLAC) {
		int i;
		int numChannels = (rawAudioType.isStereo ? 2 : 1);
		int samplesPerChannel = length / ((rawAudioType.bitsPerSample / 8) * numChannels);
		FLAC__StreamEncoder *encoder;
		FLAC__StreamEncoderInitStatus initStatus;
		FLAC__int32 *flacData;

		flacData = (FLAC__int32 *)malloc(samplesPerChannel * numChannels * sizeof(FLAC__int32));

		if (rawAudioType.bitsPerSample == 8) {
			for (i = 0; i < samplesPerChannel * numChannels; i++) {
				FLAC__uint8 *rawDataUnsigned;
				rawDataUnsigned = (FLAC__uint8 *)rawData;
				flacData[i] = (FLAC__int32)rawDataUnsigned[i] - 0x80;
			}
		} else if (rawAudioType.bitsPerSample == 16) {
			/* The rawData pointer is an 8-bit char so we must create a new pointer to access 16-bit samples */
			FLAC__int16 *rawData16;
			rawData16 = (FLAC__int16 *)rawData;
			for (i = 0; i < samplesPerChannel * numChannels; i++) {
				flacData[i] = (FLAC__int32)rawData16[i];
			}
		}

		if (!flacparms.silent) {
			print("Encoding to\n         \"%s\"\nat compression level %d using blocksize %d\n\n", outname, flacparms.compressionLevel, flacparms.blocksize);
		}

		encoder = FLAC__stream_encoder_new();

		FLAC__stream_encoder_set_bits_per_sample(encoder, rawAudioType.bitsPerSample);
		FLAC__stream_encoder_set_blocksize(encoder, flacparms.blocksize);
		FLAC__stream_encoder_set_channels(encoder, numChannels);
		FLAC__stream_encoder_set_compression_level(encoder, flacparms.compressionLevel);
		FLAC__stream_encoder_set_sample_rate(encoder, samplerate);
		FLAC__stream_encoder_set_streamable_subset(encoder, false);
		FLAC__stream_encoder_set_total_samples_estimate(encoder, samplesPerChannel);
		FLAC__stream_encoder_set_verify(encoder, flacparms.verify);

		initStatus = FLAC__stream_encoder_init_file(encoder, outname, NULL, NULL);

		if (initStatus != FLAC__STREAM_ENCODER_INIT_STATUS_OK) {
			char buf[2048];
			sprintf(buf, "Error in FLAC encoder. (check the parameters)\nExact error was:%s\n", FLAC__StreamEncoderInitStatusString[initStatus]);
			throw ToolException(buf);
		} else {
			FLAC__stream_encoder_process_interleaved(encoder, flacData, samplesPerChannel);
		}

		FLAC__stream_encoder_finish(encoder);
		FLAC__stream_encoder_delete(encoder);

		free(flacData);

		if (!flacparms.silent) {
			print("\nDone encoding file \"%s\"\n", outname);
			print("\n\tFile length:  %dm %ds\n\n", (int)(samplesPerChannel / samplerate / 60), (samplesPerChannel / samplerate % 60));
		}
	}
#endif
}

void CompressionTool::extractAndEncodeWAV(const char *outName, File &input, AudioFormat compMode) {
	unsigned int length;
	char fbuf[2048];
	size_t size;

	input.seek(-4, SEEK_CUR);
	length = input.readUint32LE();
	length += 8;
	input.seek(-8, SEEK_CUR);

	/* Copy the WAV data to a temporary file */
	File f(outName, "wb");
	while (length > 0) {
		size = input.readN(fbuf, 1, length > sizeof(fbuf) ? sizeof(fbuf) : length);
		if (size <= 0)
			break;
		length -= (int)size;
		f.write(fbuf, 1, size);
	}
	f.close();

	/* Convert the WAV temp file to OGG/MP3 */
	encodeAudio(outName, false, -1, tempEncoded, compMode);
}

void CompressionTool::extractAndEncodeVOC(const char *outName, File &input, AudioFormat compMode) {
	int bits;
	int blocktype;
	int channels;
	unsigned int length;
	int sample_rate;
	int comp;
	char fbuf[2048];
	size_t size;
	int real_samplerate = -1;

	File f(outName, "wb");

	while ((blocktype = input.readByte())) {
		if (blocktype != 1 && blocktype != 9) {
			/*
			   We only generate a warning, instead of erroring out, because
			   at least the monster.sou file of Full Throttle contains VOCs
			   with an invalid length field (value to small). So we encounter
			   the "block types" 0x80, 0x82 etc.. Not sure if there is another
			   (maybe even better) way to work around that... ?
			 */
			warning("Unsupported VOC block type: %02x", blocktype);
			break;
		}

		/* Sound Data */
		print(" Sound Data\n");
		length = input.readChar();
		length |= input.readChar() << 8;
		length |= input.readChar() << 16;

		if (blocktype == 1) {
			length -= 2;
			sample_rate = input.readByte();
			comp = input.readByte();
			real_samplerate = getSampleRateFromVOCRate(sample_rate);
		} else { /* (blocktype == 9) */
			length -= 12;
			real_samplerate = sample_rate = input.readUint32LE();
			bits = input.readChar();;
			channels = input.readChar();;
			if (bits != 8 || channels != 1) {
				error("Unsupported VOC file format (%d bits per sample, %d channels)", bits, channels);
			}
			comp = input.readUint16LE();
			input.readUint32LE();
		}

		print(" - length = %d\n", length);
		print(" - sample rate = %d (%02x)\n", real_samplerate, sample_rate);
		print(" - compression = %s (%02x)\n",
			   (comp ==	   0 ? "8bits"   :
				(comp ==   1 ? "4bits"   :
				 (comp ==  2 ? "2.6bits" :
				  (comp == 3 ? "2bits"   :
								"Multi")))), comp);

		if (comp != 0) {
			error("Cannot handle compressed VOC data");
		}

		/* Copy the raw data to a temporary file */
		while (length > 0) {
			size = input.readN(fbuf, 1, length > sizeof(fbuf) ? sizeof(fbuf) : (uint32)length);

			if (size <= 0) {
				break;
			}

			length -= (int)size;
			f.write(fbuf, 1, size);
		}
	}

	f.close();

	assert(real_samplerate != -1);

	setRawAudioType(false, false, 8);

	/* Convert the raw temp file to OGG/MP3 */
	encodeAudio(outName, true, real_samplerate, tempEncoded, compMode);
}

bool CompressionTool::processMp3Parms() {
	while (_arguments_parsed < _arguments.size()) {
		std::string arg = _arguments[_arguments_parsed];

		if (arg == "--vbr") {
			encparms.abr = 0;
		} else if (arg == "--abr") {
			encparms.abr = 1;
		} else if (arg == "-b") {
			++_arguments_parsed;
			if (_arguments_parsed >= _arguments.size())
				throw ToolException("Could not parse command line options, expected value after -b");
			encparms.minBitr = atoi(_arguments[_arguments_parsed].c_str());

			if (encparms.minBitr > 160)
				throw ToolException("Minimum bitrate out of bounds (-b), must be between 8 and 160.");

			if (encparms.minBitr == 0 && _arguments[_arguments_parsed] != "0")
				throw ToolException("Minimum bitrate (-b) must be a number.");

			if (encparms.minBitr < 8)
				throw ToolException("Minimum bitrate out of bounds (-b), must be between 8 and 160.");

		} else if (arg == "-B") {
			++_arguments_parsed;
			if (_arguments_parsed >= _arguments.size())
				throw ToolException("Could not parse command line options, expected value after -B");
			encparms.maxBitr = atoi(_arguments[_arguments_parsed].c_str());

			if ((encparms.maxBitr % 8) != 0) {
				encparms.maxBitr -= encparms.maxBitr % 8;
			}

			if (encparms.maxBitr > 160)
				throw ToolException("Maximum bitrate out of bounds (-B), must be between 8 and 160.");

			if (encparms.maxBitr == 0 && _arguments[_arguments_parsed] != "0")
				throw ToolException("Maximum bitrate (-B) must be a number.");

			if (encparms.maxBitr < 8)
				throw ToolException("Maximum bitrate out of bounds (-B), must be between 8 and 160.");

		} else if (arg == "-V") {
			++_arguments_parsed;
			if (_arguments_parsed >= _arguments.size())
				throw ToolException("Could not parse command line options, expected value after -V");
			encparms.vbrqual = atoi(_arguments[_arguments_parsed].c_str());

			if (encparms.vbrqual < 0)
				throw ToolException("Quality (-q) out of bounds, must be between 0 and 9.");

			if (encparms.vbrqual > 9)
				throw ToolException("Quality (-q) out of bounds, must be between 0 and 9.");

		} else if (arg == "-q") {
			++_arguments_parsed;
			if (_arguments_parsed >= _arguments.size())
				throw ToolException("Could not parse command line options, expected value after -q");
			encparms.algqual = atoi(_arguments[_arguments_parsed].c_str());

			if (encparms.algqual < 0)
				throw ToolException("Quality (-q) out of bounds, must be between 0 and 9.");

			if (encparms.algqual > 9)
				throw ToolException("Quality (-q) out of bounds, must be between 0 and 9.");

		} else if (arg == "--silent") {
			encparms.silent = 1;
		} else {
			break;
		}

		++_arguments_parsed;
	}

	return true;
}

bool CompressionTool::processOggParms() {
	while (_arguments_parsed < _arguments.size()) {
		std::string arg = _arguments[_arguments_parsed];

		if (arg == "-b") {
			++_arguments_parsed;
			if (_arguments_parsed >= _arguments.size())
				throw ToolException("Could not parse command line options, expected value after -b");
			oggparms.nominalBitr = atoi(_arguments[_arguments_parsed].c_str());

			if ((oggparms.nominalBitr % 8) != 0)
				oggparms.nominalBitr -= oggparms.nominalBitr % 8;

			if (oggparms.nominalBitr > 160)
				throw ToolException("Nominal bitrate out of bounds (-b), must be between 8 and 160.");

			if (oggparms.nominalBitr == 0 && _arguments[_arguments_parsed] != "0")
				throw ToolException("Nominal bitrate (-b) must be a number.");

			if (oggparms.nominalBitr < 8)
				throw ToolException("Nominal bitrate out of bounds (-b), must be between 8 and 160.");

		} else if (arg == "-m") {
			++_arguments_parsed;
			if (_arguments_parsed >= _arguments.size())
				throw ToolException("Could not parse command line options, expected value after -m");
			oggparms.minBitr = atoi(_arguments[_arguments_parsed].c_str());

			if ((oggparms.minBitr % 8) != 0)
				oggparms.minBitr -= oggparms.minBitr % 8;

			if (oggparms.minBitr > 160)
				throw ToolException("Minimal bitrate out of bounds (-m), must be between 8 and 160.");

			if (oggparms.minBitr == 0 && _arguments[_arguments_parsed] != "0")
				throw ToolException("Minimal bitrate (-m) must be a number.");

			if (oggparms.minBitr < 8)
				throw ToolException("Minimal bitrate out of bounds (-m), must be between 8 and 160.");

		} else if (arg == "-M") {
			++_arguments_parsed;
			if (_arguments_parsed >= _arguments.size())
				throw ToolException("Could not parse command line options, expected value after -M");
			oggparms.maxBitr = atoi(_arguments[_arguments_parsed].c_str());

			if ((oggparms.maxBitr % 8) != 0)
				oggparms.maxBitr -= oggparms.maxBitr % 8;

			if (oggparms.maxBitr > 160)
				throw ToolException("Minimal bitrate out of bounds (-M), must be between 8 and 160.");

			if (oggparms.maxBitr == 0 && _arguments[_arguments_parsed] != "0")
				throw ToolException("Minimal bitrate (-M) must be a number.");

			if (oggparms.maxBitr < 8)
				throw ToolException("Minimal bitrate out of bounds (-M), must be between 8 and 160.");
		} else if (arg == "-q") {
			++_arguments_parsed;
			oggparms.quality = (float)atoi(_arguments[_arguments_parsed].c_str());

			if (oggparms.quality == 0 && _arguments[_arguments_parsed] != "0")
				throw ToolException("Quality (-q) must be a number.");
		} else if (arg == "--silent") {
			oggparms.silent = 1;
		} else {
			break;
		}

		++_arguments_parsed;
	}

	return true;
}

bool CompressionTool::processFlacParms(){
	while (_arguments_parsed < _arguments.size()) {
		std::string arg = _arguments[_arguments_parsed];

		if (arg == "-b") {
			++_arguments_parsed;
			if (_arguments_parsed >= _arguments.size())
				throw ToolException("Could not parse command line options, expected value after -b");
			flacparms.blocksize = atoi(_arguments[_arguments_parsed].c_str());
		} else if (arg == "--fast") {
			flacparms.compressionLevel = 0;
		} else if (arg == "--best") {
			flacparms.compressionLevel = 8;
		} else if (arg == "-0") {
			flacparms.compressionLevel = 0;
		} else if (arg == "-1") {
			flacparms.compressionLevel = 1;
		} else if (arg == "-2") {
			flacparms.compressionLevel = 2;
		} else if (arg == "-3") {
			flacparms.compressionLevel = 3;
		} else if (arg == "-4") {
			flacparms.compressionLevel = 4;
		} else if (arg == "-5") {
			flacparms.compressionLevel = 5;
		} else if (arg == "-6") {
			flacparms.compressionLevel = 6;
		} else if (arg == "-7") {
			flacparms.compressionLevel = 7;
		} else if (arg == "-8") {
			flacparms.compressionLevel = 8;
		} else if (arg == "--verify") {
			flacparms.verify = true;
		} else if (arg == "--silent") {
			flacparms.silent = true;
		} else {
			break;
		}

		++_arguments_parsed;
	}

	return true;
}

// Compression tool interface
// Duplicates code above in the new way
// The old code can be removed once all tools have been converted

CompressionTool::CompressionTool(const std::string &name, ToolType type) : Tool(name, type) {
	_format = AUDIO_MP3;
}

void CompressionTool::parseAudioArguments() {
	_format = AUDIO_MP3;

	if (_arguments[_arguments_parsed] ==  "--mp3")
		_format = AUDIO_MP3;
	else if (_arguments[_arguments_parsed] == "--vorbis")
		_format = AUDIO_VORBIS;
	else if (_arguments[_arguments_parsed] == "--flac")
		_format = AUDIO_FLAC;
	else
		// No audio arguments then
		return;

	++_arguments_parsed;

	// Need workaround to be sign-correct
	switch (_format) {
	case AUDIO_MP3:
		tempEncoded = TEMP_MP3;
		if (!processMp3Parms())
			throw ToolException("Could not parse command line arguments, use --help for options");
		break;
	case AUDIO_VORBIS:
		tempEncoded = TEMP_OGG;
		if (!processOggParms())
			throw ToolException("Could not parse command line arguments, use --help for options");
		break;
	case AUDIO_FLAC:
		tempEncoded = TEMP_FLAC;
		if (!processFlacParms())
			throw ToolException("Could not parse arguments: Use --help for options");
		break;
	default: // cannot occur but we check anyway to avoid compiler warnings
		throw ToolException("Unknown audio format, should be impossible!");
	}
}

std::string CompressionTool::getHelp() const {
	std::ostringstream os;
	
	// Standard help text + our additions
	os << Tool::getHelp();

	if (_supportedFormats == AUDIO_NONE)
		return os.str();

	os << "\nParams:\n";

	if (_supportedFormats & AUDIO_MP3)
		os << " --mp3        encode to MP3 format (default)\n";
	if (_supportedFormats & AUDIO_VORBIS)
		os << " --vorbis     encode to Vorbis format\n";
	if (_supportedFormats & AUDIO_FLAC)
		os << " --flac       encode to Flac format\n";
	os << "(If one of these is specified, it must be the first parameter.)\n";

	if (_supportedFormats & AUDIO_MP3) {
		os << "\nMP3 mode params:\n";
		os << " -b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:" << minBitrDef << "%d)\n";
		os << " -B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%" << maxBitrDef << ")\n";
		os << " --vbr        LAME uses the VBR mode (default)\n";
		os << " --abr        LAME uses the ABR mode\n";
		os << " -V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:" << vbrqualDef << "%d)\n";
		os << " -q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:" << algqualDef << ")\n";
		os << " --silent     the output of LAME is hidden (default:disabled)\n";
	}

	if (_supportedFormats & AUDIO_VORBIS) {
		os << "\nVorbis mode params:\n";
		os << " -b <rate>    <rate> is the nominal bitrate (default:unset)\n";
		os << " -m <rate>    <rate> is the minimum bitrate (default:unset)\n";
		os << " -M <rate>    <rate> is the maximum bitrate (default:unset)\n";
		os << " -q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:" << oggqualDef << ")\n";
		os << " --silent     the output of oggenc is hidden (default:disabled)\n";
	}

	if (_supportedFormats & AUDIO_FLAC) {
		os << "\nFlac mode params:\n";
		os << " --fast       FLAC uses compression level 0\n";
		os << " --best       FLAC uses compression level 8\n";
		os << " -<value>     specifies the value (0 - 8) of compression (8=best)(default:" << flacCompressDef << ")\n";
		os << " -b <value>   specifies a blocksize of <value> samples (default:" << flacBlocksizeDef << ")\n";
		os << " --verify     files are encoded and then decoded to check accuracy\n";
		os << " --silent     the output of FLAC is hidden (default:disabled)\n";
	}

	os << "\n --help     this help message\n";

	os << "\n\nIf a parameter is not given the default value is used\n";
	os << "If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n";

	return os.str();
}
