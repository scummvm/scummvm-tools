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

void setRawAudioType(bool isLittleEndian, bool isStereo, uint8 bitsPerSample) {
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

void encodeAudio(const char *inname, bool rawInput, int rawSamplerate, const char *outname, CompressMode compmode) {
	bool err = false;
	char fbuf[2048];
	char *tmp = fbuf;

	if (compmode == kMP3Mode) {
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

		err = system(fbuf) != 0;

		if (err) {
			printf("Got error from encoder. (check your parameters)\n");
			printf("Encoder Commandline: %s\n", fbuf );
			exit(-1);
		} else {
			return;
		}
	}

#ifdef DISABLE_BUILTIN_VORBIS
		if (compmode == kVorbisMode) {
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

			err = system(fbuf) != 0;

			if (err) {
				printf("Got error from encoder. (check your parameters)\n");
				printf("Encoder Commandline: %s\n", fbuf );
				exit(-1);
			} else {
				return;
			}
		}
#endif

#ifdef DISABLE_BUILTIN_FLAC
		if (compmode == kFlacMode) {
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

			err = system(fbuf) != 0;

			if (err) {
				printf("Got error from encoder. (check your parameters)\n");
				printf("Encoder Commandline: %s\n", fbuf );
				exit(-1);
			} else {
				return;
			}
		}
#endif
		if (rawInput) {
			FILE *inputRaw;
			long length;
			char *rawData;

			inputRaw = fopen(inname, "rb");
			length = fileSize(inputRaw);
			rawData = (char *)malloc(length);
			fread(rawData, 1, length, inputRaw);

			printf(" - length = %ld\n", length);
			printf(" - channels = %d\n", (rawAudioType.isStereo ? 2 : 1));
			printf(" - sample rate = %d\n", rawSamplerate);
			printf(" - compression = %dbits\n", rawAudioType.bitsPerSample);

			encodeRaw(rawData, length, rawSamplerate, outname, compmode);

			fclose(inputRaw);
			free(rawData);
		} else {
			FILE *inputWav;
			int fmtHeaderSize, length, numChannels, sampleRate, bitsPerSample;
			char *wavData;

			inputWav = fopen(inname, "rb");

			/* Standard PCM fmt header is 16 bits, but at least Simon 1 and 2 use 18 bits */
			fseek(inputWav, 16, SEEK_SET);
			fmtHeaderSize = readUint32LE(inputWav);

			fseek(inputWav, 22, SEEK_SET);
			numChannels = readUint16LE(inputWav);
			sampleRate = readUint32LE(inputWav);

			fseek(inputWav, 34, SEEK_SET);
			bitsPerSample = readUint16LE(inputWav);

			/* The size of the raw audio is after the RIFF chunk (12 bytes), fmt chunk (8 + fmtHeaderSize bytes), and data chunk id (4 bytes) */
			fseek(inputWav, 24 + fmtHeaderSize, SEEK_SET);
			length = readUint32LE(inputWav);

			wavData = (char *)malloc(length);
			fread(wavData, 1, length, inputWav);

			printf(" - length = %d\n", length);
			printf(" - channels = %d\n", numChannels);
			printf(" - sample rate = %d\n", sampleRate);
			printf(" - compression = %dbits\n", bitsPerSample);

			setRawAudioType(true, numChannels == 2, bitsPerSample);
			encodeRaw(wavData, length, sampleRate, outname, compmode);

			fclose(inputWav);
			free (wavData);
		}
}

void encodeRaw(char *rawData, int length, int samplerate, const char *outname, CompressMode compmode) {
#ifndef DISABLE_BUILTIN_VORBIS
	if (compmode == kVorbisMode) {
		FILE *outputOgg;
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

		outputOgg = fopen(outname,"wb");

		vorbis_info_init(&vi);

		if (oggparms.nominalBitr > 0) {
			int result = 0;

			/* Input is in kbps, function takes bps */
			result = vorbis_encode_setup_managed(&vi, numChannels, samplerate, (oggparms.maxBitr > 0 ? 1000 * oggparms.maxBitr : -1), (1000 * oggparms.nominalBitr), (oggparms.minBitr > 0 ? 1000 * oggparms.minBitr : -1));

			if (result == OV_EFAULT) {
				printf("Error: Internal Logic Fault.\n\n");
				vorbis_info_clear(&vi);
				exit(-1);
			} else if ((result == OV_EINVAL) || (result == OV_EIMPL)) {
				printf("Error: Invalid bitrate parameters.\n\n");
				vorbis_info_clear(&vi);
				exit(-1);
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
				printf("Error: Internal Logic Fault.\n\n");
				vorbis_info_clear(&vi);
				exit(-1);
			} else if ((result == OV_EINVAL) || (result == OV_EIMPL)) {
				printf("Error: Invalid bitrate parameters.\n\n");
				vorbis_info_clear(&vi);
				exit(-1);
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

		printf(outputString);

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

			fwrite(og.header, 1, og.header_len, outputOgg);
			fwrite(og.body, 1, og.body_len, outputOgg);
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
						for(i = 0; i < numSamples; i++) {
							for(j = 0; j < numChannels; j++) {
								buffer[j][i] = ((rawData[(i * 2 * numChannels) + (2 * j) + 1] << 8) | (rawData[(i * 2 * numChannels) + (2 * j)] & 0xff)) / 32768.0f;
							}
						}
					}
					else {
						for(i = 0; i < numSamples; i++) {
							for(j = 0; j < numChannels; j++) {
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

						totalBytes += fwrite(og.header, 1, og.header_len, outputOgg);
						totalBytes += fwrite(og.body, 1, og.body_len, outputOgg);

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

		fclose(outputOgg);

		if (!oggparms.silent) {
			printf("\nDone encoding file \"%s\"\n", outname);
			printf("\n\tFile length:  %dm %ds\n", (int)(totalSamples / samplerate / 60), (totalSamples / samplerate % 60));
			printf("\tAverage bitrate: %.1f kb/s\n\n", (8.0 * (double)totalBytes / 1000.0) / ((double)totalSamples / (double)samplerate));
		}
	}
#endif

#ifndef DISABLE_BUILTIN_FLAC
	if (compmode == kFlacMode) {
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
			printf("Encoding to\n         \"%s\"\nat compression level %d using blocksize %d\n\n", outname, flacparms.compressionLevel, flacparms.blocksize);
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
			printf("Got error from encoder. (check your paramters)\n");
			printf("FLAC error: %s\n\n", FLAC__StreamEncoderInitStatusString[initStatus]);
			exit(-1);
		} else {
			FLAC__stream_encoder_process_interleaved(encoder, flacData, samplesPerChannel);
		}

		FLAC__stream_encoder_finish(encoder);
		FLAC__stream_encoder_delete(encoder);

		free(flacData);

		if (!flacparms.silent) {
			printf("\nDone encoding file \"%s\"\n", outname);
			printf("\n\tFile length:  %dm %ds\n\n", (int)(samplesPerChannel / samplerate / 60), (samplesPerChannel / samplerate % 60));
		}
	}
#endif
}

void extractAndEncodeWAV(const char *outName, FILE *input, CompressMode compMode) {
	unsigned int length;
	FILE *f;
	char fbuf[2048];
	size_t size;

	fseek(input, -4, SEEK_CUR);
	length = readUint32LE(input);
	length += 8;
	fseek(input, -8, SEEK_CUR);

	/* Copy the WAV data to a temporary file */
	f = fopen(outName, "wb");
	while (length > 0) {
		size = fread(fbuf, 1, length > sizeof(fbuf) ? sizeof(fbuf) : length, input);
		if (size <= 0)
			break;
		length -= (int)size;
		fwrite(fbuf, 1, size, f);
	}
	fclose(f);

	/* Convert the WAV temp file to OGG/MP3 */
	encodeAudio(outName, false, -1, tempEncoded, compMode);
}

void extractAndEncodeVOC(const char *outName, FILE *input, CompressMode compMode) {
	FILE *f;
	int bits;
	int blocktype;
	int channels;
	unsigned int length;
	int sample_rate;
	int comp;
	char fbuf[2048];
	size_t size;
	int real_samplerate = -1;

	f = fopen(outName, "wb");

	while ((blocktype = fgetc(input))) {
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
		printf(" Sound Data\n");
		length = fgetc(input);
		length |= fgetc(input) << 8;
		length |= fgetc(input) << 16;

		if (blocktype == 1) {
			length -= 2;
			sample_rate = fgetc(input);
			comp = fgetc(input);
			real_samplerate = getSampleRateFromVOCRate(sample_rate);
		} else { /* (blocktype == 9) */
			length -= 12;
			real_samplerate = sample_rate = readUint32LE(input);
			bits = fgetc(input);
			channels = fgetc(input);
			if (bits != 8 || channels != 1) {
				error("Unsupported VOC file format (%d bits per sample, %d channels)", bits, channels);
			}
			comp = readUint16LE(input);
			readUint32LE(input);
		}

		printf(" - length = %d\n", length);
		printf(" - sample rate = %d (%02x)\n", real_samplerate, sample_rate);
		printf(" - compression = %s (%02x)\n",
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
			size = fread(fbuf, 1, length > sizeof(fbuf) ? sizeof(fbuf) : (uint32)length, input);

			if (size <= 0) {
				break;
			}

			length -= (int)size;
			fwrite(fbuf, 1, size, f);
		}
	}

	fclose(f);

	assert(real_samplerate != -1);

	setRawAudioType(false, false, 8);

	/* Convert the raw temp file to OGG/MP3 */
	encodeAudio(outName, true, real_samplerate, tempEncoded, compMode);
}

int process_mp3_parms(int argc, char *argv[], int* i) {
	for (; *i < argc; (*i)++) {
		if (strcmp(argv[*i], "--vbr") == 0) {
			encparms.abr = 0;
		} else if (strcmp(argv[*i], "--abr") == 0) {
			encparms.abr = 1;
		} else if (strcmp(argv[*i], "-b") == 0) {
			encparms.minBitr = atoi(argv[*i + 1]);

			if ((encparms.minBitr % 8) != 0) {
				encparms.minBitr -= encparms.minBitr % 8;
			}

			if (encparms.minBitr > 160) {
				encparms.minBitr = 160;
			}

			if (encparms.minBitr < 8) {
				encparms.minBitr = 8;
			}

			(*i)++;
		} else if (strcmp(argv[*i], "-B") == 0) {
			encparms.maxBitr = atoi(argv[*i + 1]);

			if ((encparms.maxBitr % 8) != 0) {
				encparms.maxBitr -= encparms.maxBitr % 8;
			}

			if (encparms.maxBitr > 160) {
				encparms.maxBitr = 160;
			}

			if (encparms.maxBitr < 8) {
				encparms.maxBitr = 8;
			}

			(*i)++;
		} else if (strcmp(argv[*i], "-V") == 0) {
			encparms.vbrqual = atoi(argv[*i + 1]);

			if (encparms.vbrqual < 0) {
				encparms.vbrqual = 0;
			}

			if (encparms.vbrqual > 9) {
				encparms.vbrqual = 9;
			}

			(*i)++;
		} else if (strcmp(argv[*i], "-q") == 0) {
			encparms.algqual = atoi(argv[*i + 1]);

			if (encparms.algqual < 0) {
				encparms.algqual = 0;
			}

			if (encparms.algqual > 9) {
				encparms.algqual = 9;
			}

			(*i)++;
		} else if (strcmp(argv[*i], "--silent") == 0) {
			encparms.silent = 1;
		} else if (strcmp(argv[*i], "--help") == 0) {
			return 0;
		} else if (argv[*i][0] == '-') {
			return 0;
		} else {
			break;
		}
	}

	if (*i != (argc - 1)) {
		return 0;
	}

	return 1;
}

int process_ogg_parms(int argc, char *argv[], int* i) {
	for (; *i < argc; (*i)++) {
		if (strcmp(argv[*i], "-b") == 0) {
			oggparms.nominalBitr = atoi(argv[*i + 1]);

			if ((oggparms.nominalBitr % 8) != 0) {
				oggparms.nominalBitr -= oggparms.nominalBitr % 8;
			}

			if (oggparms.nominalBitr >160) {
				oggparms.nominalBitr = 160;
			}

			if (oggparms.nominalBitr < 8) {
				oggparms.nominalBitr = 8;
			}

			(*i)++;
		} else if (strcmp(argv[*i], "-m") == 0) {
			oggparms.minBitr = atoi(argv[*i + 1]);

			if ((oggparms.minBitr % 8) != 0) {
				oggparms.minBitr -= oggparms.minBitr % 8;
			}

			if (oggparms.minBitr >160) {
				oggparms.minBitr = 160;
			}

			if (oggparms.minBitr < 8) {
				oggparms.minBitr = 8;
			}

			(*i)++;
		} else if (strcmp(argv[*i], "-M") == 0) {
			oggparms.maxBitr = atoi(argv[*i + 1]);

			if ((oggparms.maxBitr % 8) != 0) {
				oggparms.maxBitr -= encparms.minBitr % 8;
			}

			if (oggparms.maxBitr >160) {
				oggparms.maxBitr = 160;
			}

			if (oggparms.maxBitr < 8) {
				oggparms.maxBitr = 8;
			}

			(*i)++;
		} else if (strcmp(argv[*i], "-q") == 0) {
			oggparms.quality = (float)atoi(argv[*i + 1]);
			(*i)++;
		} else if (strcmp(argv[*i], "--silent") == 0) {
			oggparms.silent = 1;
		} else if (strcmp(argv[*i], "--help") == 0) {
			return 0;
		} else if (argv[*i][0] == '-') {
			return 0;
		} else {
			break;
		}
	}

	if (*i != argc - 1) {
		return 0;
	}

	return 1;
}

int process_flac_parms(int argc, char *argv[], int *i){
	for (; *i < argc; (*i)++) {
		if (strcmp(argv[*i], "-b") == 0) {
			flacparms.blocksize = atoi(argv[*i + 1]);
			(*i)++;
		} else if (strcmp(argv[*i], "--fast") == 0) {
			flacparms.compressionLevel = 0;
		} else if (strcmp(argv[*i], "--best") == 0) {
			flacparms.compressionLevel = 8;
		} else if (strcmp(argv[*i], "-0") == 0) {
			flacparms.compressionLevel = 0;
		} else if (strcmp(argv[*i], "-1") == 0) {
			flacparms.compressionLevel = 1;
		} else if (strcmp(argv[*i], "-2") == 0) {
			flacparms.compressionLevel = 2;
		} else if (strcmp(argv[*i], "-3") == 0) {
			flacparms.compressionLevel = 3;
		} else if (strcmp(argv[*i], "-4") == 0) {
			flacparms.compressionLevel = 4;
		} else if (strcmp(argv[*i], "-5") == 0) {
			flacparms.compressionLevel = 5;
		} else if (strcmp(argv[*i], "-6") == 0) {
			flacparms.compressionLevel = 6;
		} else if (strcmp(argv[*i], "-7") == 0) {
			flacparms.compressionLevel = 7;
		} else if (strcmp(argv[*i], "-8") == 0) {
			flacparms.compressionLevel = 8;
		} else if (strcmp(argv[*i], "--verify") == 0) {
			flacparms.verify = true;
		} else if (strcmp(argv[*i], "--silent") == 0) {
			flacparms.silent = true;
		} else if (strcmp(argv[*i], "--help") == 0) {
			return 0;
		} else if (argv[*i][0] == '-') {
			return 0;
		} else {
			break;
		}
	}

	if (*i != argc - 1) {
		return 0;
	}

	return 1;
}

CompressMode process_audio_params(int argc, char *argv[], int* i) {
	/* Compression mode */
	CompressMode compMode = kMP3Mode;

	for (; *i < argc - 2; ++*i) {
		if (strcmp(argv[*i], "--mp3") == 0)
			compMode = kMP3Mode;
		else if (strcmp(argv[*i], "--vorbis") == 0)
			compMode = kVorbisMode;
		else if (strcmp(argv[*i], "--flac") == 0)
			compMode = kFlacMode;
		else
			break;
	}

	switch (compMode) {
	case kMP3Mode:
		tempEncoded = TEMP_MP3;
		if (!process_mp3_parms(argc - 2, argv, i))
			return kNoAudioMode;
		break;
	case kVorbisMode:
		tempEncoded = TEMP_OGG;
		if (!process_ogg_parms(argc - 2, argv, i))
			return kNoAudioMode;
		break;
	case kFlacMode:
		tempEncoded = TEMP_FLAC;
		if (!process_flac_parms(argc - 2, argv, i))
			return kNoAudioMode;
		break;
	}

	return compMode;
}
