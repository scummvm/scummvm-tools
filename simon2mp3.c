#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

/* These are the defaults parameters for the Lame invocation */
#define minBitrDef 24
#define maxBitrDef 64
#define abrDef 0
#define vbrDef 1
#define algqualDef 2
#define vbrqualDef 4

/* The default for oggenc invocation is to use the --quality option only */
#define oggqualDef 3

FILE *input, *output_idx, *output_snd;

unsigned int offsets[32768];

char infile_base[256];
char fbuf_temp[1024];
char buf[256];
char tmp[256];

struct lameparams {
	unsigned int minBitr;
	unsigned int maxBitr; 
	unsigned int abr;
	unsigned int vbr;
	unsigned int algqual;
	unsigned int vbrqual;
	unsigned int silent;
} encparms = { minBitrDef, maxBitrDef, abrDef, vbrDef, algqualDef, vbrqualDef, 0 };

struct oggencparams {
	int nominalBitr;
	int minBitr;
	int maxBitr;
	int quality;
	int silent;
} oggparms = { -1, -1, -1, oggqualDef, 0 };

int oggmode = 0;

void end(void)
{
	int size;
	char buf[2048];

	fclose(output_snd);
	fclose(output_idx);
	fclose(input);

	sprintf(tmp, "%s%s", infile_base, oggmode ? "ogg" : "mp3");
	output_idx = fopen(tmp, "wb");

	sprintf(tmp, "%sidx", infile_base);
	input = fopen(tmp, "rb");
	while ((size = fread(buf, 1, 2048, input)) > 0) {
		fwrite(buf, 1, size, output_idx);
	}
	fclose(input);
	sprintf(tmp, "%sdat", infile_base);
	input = fopen(tmp, "rb");
	while ((size = fread(buf, 1, 2048, input)) > 0) {
		fwrite(buf, 1, size, output_idx);
	}
	fclose(input);
	fclose(output_idx);

	/* And some clean-up :-) */
	sprintf(tmp, "%sidx", infile_base);
	unlink(tmp);
	sprintf(tmp, "%sdat", infile_base);
	unlink(tmp);
	unlink("tempfile.raw");
	unlink(oggmode ? "tempfile.ogg" : "tempfile.mp3");
	unlink("tempfile.wav");
	
	exit(0);
}

void get_string(int size)
{
	int i = 0;
	while (i < size) {
		int c = fgetc(input);
		buf[i++] = c;
	}
	buf[i] = '\0';
}

unsigned int get_int(void)
{
	int i;
	unsigned int ret = 0;
	unsigned int c;
	for (i = 0; i < 4; i++) {
		c = fgetc(input);
		ret |= c << i*8;
	}
	return ret;
}

void put_int(unsigned int val)
{
	int i;
	for (i = 3; i >= 0; i--) {
		fputc((val << i*8) >> 24, output_idx);
	}
}

int get_offsets(void)
{
	int i;

	for (i = 0;; i++) {
		get_string(8);
		if (!strncmp(buf, "Creative", 8) || !strncmp(buf, "RIFF", 4)) {
			return(i);
		}
		fseek(input, -8, SEEK_CUR);

		offsets[i] = get_int();
	}
}

void get_voc(void);
void get_wav(void);

unsigned int get_sound(int sound)
{
	FILE *f;
	unsigned int tot_size;
	char mp3name[256];
	int size;
	char fbuf[2048];

	fseek(input, offsets[sound], SEEK_SET);

	get_string(8);
	if (!strncmp(buf, "Creative", 8)) {
		printf("VOC found (pos = %d) :\n", offsets[sound]);
		get_voc();
	} else if (!strncmp(buf, "RIFF", 4)) {
		printf("WAV found (pos = %d) :\n", offsets[sound]);
		get_wav();
	} else {
		printf("Unexpected data at offset: %i\n", offsets[sound]);
		exit(-1);
	}

	sprintf(mp3name, oggmode ? "tempfile.ogg" : "tempfile.mp3");
	f = fopen(mp3name, "rb");
	tot_size = 0;
	while ((size = fread(fbuf, 1, 2048, f)) > 0) {
		tot_size += size;
		fwrite(fbuf, 1, size, output_snd);
	}
	fclose(f);

	return(tot_size);
}

void get_wav(void) {
	int length;
	FILE *f;
	char fbuf[2048];
	int size;
	char wavname[256];
	char mp3name[256];

	fseek(input, -4, SEEK_CUR);
	length = get_int();
	length += 8;
	fseek(input, -8, SEEK_CUR);

	sprintf(wavname, "tempfile.wav");
	sprintf(mp3name, oggmode ? "tempfile.ogg" : "tempfile.mp3");

	f = fopen(wavname, "wb");
	while (length > 0) {
		size = fread(fbuf, 1, length > 2048 ? 2048 : length, input);
		if (size <= 0)
			break;
		length -= size;
		fwrite(fbuf, 1, size, f);
	}
	fclose(f);

	if (oggmode) {
		sprintf(fbuf, "oggenc ");
		if (oggparms.nominalBitr != -1) {
			sprintf(fbuf_temp, "-b %i ", oggparms.nominalBitr);
			strcat(fbuf, fbuf_temp);
		}
		if (oggparms.minBitr != -1) {
			sprintf(fbuf_temp, "-m %i ", oggparms.minBitr);
			strcat(fbuf, fbuf_temp);
		}
		if (oggparms.maxBitr != -1) {
			sprintf(fbuf_temp, "-M %i ", oggparms.maxBitr);
			strcat(fbuf, fbuf_temp);
		}
		if (oggparms.silent) {
			strcat(fbuf, "--quiet ");
		}
		sprintf(fbuf_temp, "-q %i --resample 22050 %s -o %s",
			oggparms.quality, wavname, mp3name);
		strcat(fbuf, fbuf_temp);
		system(fbuf);
	} else {
		if (encparms.abr == 1)
			sprintf(fbuf_temp,"--abr %i",encparms.minBitr);
		else
			sprintf(fbuf_temp,"--vbr-new -b %i",encparms.minBitr);
		if (encparms.silent == 1)
			strcat(fbuf_temp," --silent");
		sprintf(fbuf,
			"lame -t -q %i %s -V %i -B %i --resample 22.05 -m m %s %s",
			encparms.algqual, fbuf_temp, encparms.vbrqual,
			encparms.maxBitr, wavname, mp3name);
		system(fbuf);
	}
}

void get_voc(void)
{
	int blocktype;

	get_string(18);

	blocktype = fgetc(input);
	switch (blocktype) {
	case 0x01:{
		int length = 0;
		int i;
		int sample_rate;
		int comp;
		FILE *f;
		char fbuf[2048];
		char fbuf_o[4096];
		int size;
		char rawname[256];
		char mp3name[256];
		int real_samplerate;

		/* Sound Data */
		printf(" Sound Data\n");
		for (i = 0; i < 3; i++)
			length = length | (fgetc(input) << (i * 8));
		length -= 2;
		printf(" - length = %d\n", length);
		sample_rate = fgetc(input);
		comp = fgetc(input);
		
        	/* workaround for voc weakness */
        	if (sample_rate == 0xa6) {
                	real_samplerate = 11025;
        	} else if (sample_rate == 0xd2) {
                	real_samplerate = 22050;
        	} else {
                	real_samplerate = 1000000 / (256 - sample_rate);
        	}
		
		printf(" - sample rate = %d (%02x)\n", real_samplerate, sample_rate);
		printf(" - compression = %s (%02x)\n",
		       (comp ==	   0 ? "8bits"   :
		        (comp ==   1 ? "4bits"   :
		         (comp ==  2 ? "2.6bits" :
		          (comp == 3 ? "2bits"   :
		                        "Multi")))), comp);

		if (comp != 0) {
			printf("Cannot handle compression\n");
			exit(-1);
		}
		sprintf(rawname, "tempfile.raw");
		sprintf(mp3name, oggmode ? "tempfile.ogg" : "tempfile.mp3");
	
		f = fopen(rawname, "wb");
		while (length > 0) {
			size = fread(fbuf, 1, length > 2048 ? 2048 : length, input);
			if (size <= 0)
				break;
			length -= size;
			for (i = 0; i < size; i++) {
				fbuf_o[2 * i] = fbuf[i] ^ 0x80;
				fbuf_o[2 * i + 1] = 0;
			}
			fwrite(fbuf_o, 1, 2 * size, f);
		}
		fclose(f);

		if (oggmode) {
			sprintf(fbuf, "oggenc ");
			if (oggparms.nominalBitr != -1) {
				sprintf(fbuf_temp, "-b %i ", oggparms.nominalBitr);
				strcat(fbuf, fbuf_temp);
			}
			if (oggparms.minBitr != -1) {
				sprintf(fbuf_temp, "-m %i ", oggparms.minBitr);
				strcat(fbuf, fbuf_temp);
			}
			if (oggparms.maxBitr != -1) {
				sprintf(fbuf_temp, "-M %i ", oggparms.maxBitr);
				strcat(fbuf, fbuf_temp);
			}
			if (oggparms.silent) {
				strcat(fbuf, "--quiet ");
			}
			sprintf(fbuf_temp, "-q %i -r -C 1 --raw-endianness=1 -R %i --resample 22050 %s -o %s",
				oggparms.quality, real_samplerate,
				rawname, mp3name);
			strcat(fbuf, fbuf_temp);
			system(fbuf);
		}
		else {
			if (encparms.abr == 1)
				sprintf(fbuf_temp,"--abr %i",encparms.minBitr);
			else
				sprintf(fbuf_temp,"--vbr-new -b %i",encparms.minBitr);
			if (encparms.silent == 1)
				strcat(fbuf_temp," --silent");
			sprintf(fbuf,
				"lame -t -q %i %s -V %i -B %i --resample 22.05 -m m -r -s %d %s %s",
				encparms.algqual, fbuf_temp, encparms.vbrqual,
				encparms.maxBitr, real_samplerate, rawname, mp3name);
			system(fbuf);
		}
		break;
	}

	default:
		printf("Unknown chunk : %02x\n", blocktype);
		exit(-1);
		break;
	}
}

void showhelp(char *exename)
{
	printf("\nUsage: %s <params> file\n", exename);
	printf("\nParams:\n");
	printf("--mp3        encode to MP3 format (default)\n");
	printf("--vorbis     encode to Vorbis format (not yet implemented)\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");
	printf("\nMP3 mode params:\n");
	printf("-b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%i)\n", minBitrDef);
	printf("-B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%i)\n", maxBitrDef);
	printf("--vbr        LAME uses the VBR mode (default)\n");
	printf("--abr        LAME uses the ABR mode\n");
	printf("-V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%i)\n", vbrqualDef);
	printf("-q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%i)\n", algqualDef);
	printf("--silent     the output of LAME is hidden (default:disabled)\n");
	printf("\nVorbis mode params:\n");
	printf("-b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf("-m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf("-M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf("-q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%i)\n", oggqualDef);
	printf("--silent     the output of oggenc is hidden (default:disabled)\n");
	printf("\n--help     this help message\n");
	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	exit(2);
}

void process_mp3_parms(int argc, char *argv[], int i) {
	for(; i < argc; i++) {
		if (strcmp(argv[i], "--vbr") == 0) {
			encparms.vbr=1;
			encparms.abr=0;
		} else if (strcmp(argv[i], "--abr") == 0) {
			encparms.vbr=0;
			encparms.abr=1;
		} else if (strcmp(argv[i], "-b") == 0) {
			encparms.minBitr = atoi(argv[i + 1]);
			if ((encparms.minBitr % 8) != 0)
				encparms.minBitr -= encparms.minBitr % 8;
			if (encparms.minBitr >160)
				encparms.minBitr = 160;
			if (encparms.minBitr < 8)
				encparms.minBitr=8;
			i++;
		} else if (strcmp(argv[i], "-B") == 0) {
			encparms.maxBitr = atoi(argv[i + 1]);
			if ((encparms.maxBitr % 8) != 0)
				encparms.maxBitr -= encparms.maxBitr % 8;
			if (encparms.maxBitr > 160)
				encparms.maxBitr = 160;
			if (encparms.maxBitr < 8)
				encparms.maxBitr = 8;
			i++;
		} else if (strcmp(argv[i], "-V") == 0) {
			encparms.vbrqual = atoi(argv[i + 1]);
			if(encparms.vbrqual < 0)
				encparms.vbrqual = 0;
			if(encparms.vbrqual > 9)
				encparms.vbrqual = 9;
			i++;
		} else if (strcmp(argv[i], "-q") == 0) {
			encparms.algqual = atoi(argv[i + 1]);
			if (encparms.algqual < 0)
				encparms.algqual = 0;
			if (encparms.algqual > 9)
				encparms.algqual = 9;
			i++;
		} else if (strcmp(argv[i], "--silent") == 0) {
			encparms.silent = 1;
		} else if (strcmp(argv[i], "--help") == 0) {
			showhelp(argv[0]);
		} else if (argv[i][0] == '-') {
			showhelp(argv[0]);
		} else {
			break;
		}
	}
	if (i != (argc - 1)) {
		showhelp(argv[0]);
	}
}

void process_ogg_parms(int argc, char *argv[], int i) {
	for (; i < argc; i++) {
		if (strcmp(argv[i], "-b") == 0) {
			oggparms.nominalBitr = atoi(argv[i + 1]);
			i++;
		}
		else if (strcmp(argv[i], "-m") == 0) {
			oggparms.minBitr = atoi(argv[i + 1]);
			i++;
		}
		else if (strcmp(argv[i], "-M") == 0) {
			oggparms.maxBitr = atoi(argv[i + 1]);
			i++;
		}
		else if (strcmp(argv[i], "-q") == 0) {
			oggparms.quality = atoi(argv[i + 1]);
			i++;
		}
		else if (strcmp(argv[i], "--silent") == 0) {
			oggparms.silent = 1;
		}
		else if (strcmp(argv[i], "--help") == 0) {
			showhelp(argv[0]);
		}
		else if (argv[i][0] == '-') {
			showhelp(argv[0]);
		}
		else
			break;
	}
	if (i != argc - 1)
		showhelp(argv[0]);
}

int main(int argc, char *argv[])
{
	int i, n, size, num;
	
	if (argc < 2)
		showhelp(argv[0]);
	i = 1;
	if (strcmp(argv[1], "--mp3") == 0) {
		oggmode = 0;
		i++;
	}
	else if (strcmp(argv[1], "--vorbis") == 0) {
		oggmode = 1;
		i++;
	}

	if (oggmode)
		process_ogg_parms(argc, argv, i);
	else
		process_mp3_parms(argc, argv, i);

	i = argc - 1;

	n = strlen(argv[i]);
	strncpy(infile_base, argv[i], n - 3);

	input = fopen(argv[i], "rb");
	if (!input) {
		printf("Cannot open file: %s\n", argv[i]);
		exit(-1);
	}

	sprintf(tmp, "%sidx", infile_base);
	output_idx = fopen(tmp, "wb");

	sprintf(tmp, "%sdat", infile_base);
	output_snd = fopen(tmp, "wb");

	num = get_offsets();

	if (!num) {
		printf("This does not seem to be a valid file\n");
		exit(-1);
	}
	size = num*4;

	put_int(0);
	put_int(size);

	for (i = 1; i < num; i++) {
		if (offsets[i] == offsets[i+1]) {
			put_int(size);
			continue;
		}

		size += get_sound(i);
		if (i < num - 1)
			put_int(size);
	}

	end();

	return(0);
}
