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

FILE *input, *output_idx, *output_snd;

char fbuf_temp[1024];
unsigned char buf[256];

unsigned char f_hdr[] = {
	'S', 'O', 'U', ' ', 0, 0, 0, 0, 0
};

unsigned char v_hdr[] = {
	'V', 'C', 'T', 'L', 0, 0, 0, 0xA, 0xF, 0xFF
};

unsigned char c_hdr[] = {
	'C', 'r', 'e', 'a', 't', 'i', 'v', 'e', ' ', 'V', 'o', 'i', 'c', 'e',
	' ', 'F', 'i', 'l', 'e', 0x1a, 0x1a, 0x00, 0x0A, 0x01, 0x29, 0x11
};

struct lameparams {
	unsigned int minBitr;
	unsigned int maxBitr; 
	unsigned int abr;
	unsigned int vbr;
	unsigned int algqual;
	unsigned int vbrqual;
	unsigned int silent;
} encparms = { minBitrDef, maxBitrDef, abrDef, vbrDef, algqualDef, vbrqualDef, 0};
    
void put_int(unsigned int val);

void end_of_file(void)
{
	FILE *in;
	int idx_size = ftell(output_idx);
	int size;
	char buf[2048];

	fclose(output_snd);
	fclose(output_idx);

	output_idx = fopen("monster.so3", "wb");
	put_int(idx_size);

	in = fopen("monster.idx", "rb");
	while ((size = fread(buf, 1, 2048, in)) > 0) {
		fwrite(buf, 1, size, output_idx);
	}
	fclose(in);
	in = fopen("monster.mp3", "rb");
	while ((size = fread(buf, 1, 2048, in)) > 0) {
		fwrite(buf, 1, size, output_idx);
	}
	fclose(in);
	fclose(output_idx);
	fclose(input);

	/* And some clean-up :-) */
	unlink("monster.idx");
	unlink("monster.mp3");
	unlink("tempfile.raw");
	unlink("tempfile.mp3");
	
	exit(-1);
}

void get_string(int size)
{
	int i = 0;
	while (i < size) {
		int c = fgetc(input);
		if (c == EOF)
			end_of_file();
		buf[i++] = c;
	}
	buf[i] = '\0';
}

int get_int(int size)
{
	int ret = 0;
	while (size > 0) {
		int c = fgetc(input);
		if (c == EOF)
			end_of_file();
		ret <<= 8;
		ret |= c;
		size--;
	}
	return ret;
}
void append_byte(int size)
{
	int i;
	int c;
	for (i = 0; i < (size - 1); i++)
		buf[i] = buf[i + 1];
	c = fgetc(input);
	if (c == EOF)
		end_of_file();
	buf[i] = c;
}

void put_int(unsigned int val)
{
	int i;
	for (i = 0; i < 4; i++) {
		fputc(val >> 24, output_idx);
		val <<= 8;
	}
}

void get_part(void)
{
	int id;
	int pos = ftell(input);
	int tags;

	/* The VCTL header */
	get_string(4);
	while (strncmp(buf, "VCTL", 4)) {
		pos++;
		append_byte(4);
	}
	tags = get_int(4);
	tags -= 8;

	put_int(pos);
	put_int(ftell(output_snd));
	if (tags < 0)
		exit(-1);
	put_int(tags);
	while (tags > 0) {
		fputc(fgetc(input), output_snd);
		tags--;
	}

	get_string(8);
	if (!strncmp(buf, "Creative", 8)) {
		get_string(18);
	} else if (!strncmp(buf, "VTLK", 4)) {
		get_string(26);
	} else {
		exit(-1);
	}
	printf("Voice file found (pos = %d) :", pos);

	id = fgetc(input);
	switch (id) {
	case 0x01:{
		int length = 0;
		int i;
		int sample_rate;
		int comp;
		FILE *f;
		char fbuf[2048];
		char fbuf_o[4096];
		int size;
		int tot_size;
		char rawname[256];
		char mp3name[256];
		int real_samplerate;

		/* Sound Data */
		printf(" Sound Data\n");
		for (i = 0; i < 3; i++)
			length = length | (fgetc(input) << (i * 8));
		printf(" - length = %d\n", length);
		sample_rate = fgetc(input);
		comp = fgetc(input);
		real_samplerate = 1000000 / (256 - sample_rate);
		printf(" - sample rate = %d (%02x)\n", 1000000 / (256 - sample_rate), sample_rate);
		printf(" - compression = %s (%02x)\n",
		       (comp ==	   0 ? "8bits"   :
		        (comp ==   1 ? "4bits"   :
		         (comp ==  2 ? "2.6bits" :
		          (comp == 3 ? "2bits"   :
		                        "Multi")))), comp);

		if (comp != 0) {
			exit(-1);
		}
		sprintf(rawname, "tempfile.raw");
		sprintf(mp3name, "tempfile.mp3");
		
		f = fopen(rawname, "wb");
		length -= 2;
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
		
		if (encparms.abr == 1)
			sprintf(fbuf_temp,"--abr %i",encparms.minBitr);
		else
                	sprintf(fbuf_temp,"--vbr-new -b %i",encparms.minBitr);
		if (encparms.silent == 1)
                	strcat(fbuf_temp," --silent");
		sprintf(fbuf,
		        "lame -t -q %i %s -V %i -B %i --resample 22.05 -m m --bitwidth 16 -r -s %d %s %s",
			encparms.algqual, fbuf_temp, encparms.vbrqual,
		        encparms.maxBitr, real_samplerate, rawname, mp3name);
		system(fbuf);

		f = fopen(mp3name, "rb");
		tot_size = 0;
		while ((size = fread(fbuf, 1, 2048, f)) > 0) {
			tot_size += size;
			fwrite(fbuf, 1, size, output_snd);
		}
		fclose(f);
		put_int(tot_size);
	} break;

	default:
		printf("Unknown chunk : %02x\n", id);
		exit(-1);
		break;
	}
}

void showhelp(char *exename)
{
	printf("\nUsage: %s <params> monster.sou\n", exename);
	printf("\nParams:\n");
	printf("-b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%i)\n", minBitrDef);
	printf("-B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%i)\n", maxBitrDef);
	printf("--vbr        LAME uses the VBR mode (default)\n");
	printf("--abr        LAME uses the ABR mode\n");
	printf("-V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%i)\n", vbrqualDef);
	printf("-q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%i)\n", algqualDef);
	printf("--silent     the output of LAME is hidden (default:disabled)\n");
	printf("--help       this help message\n");
	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode -b and -B must be multiples of 8; the maximum is 160!\n");
	exit(2);
}

int main(int argc, char *argv[])
{
	int i;
	if (argc < 2)
	  	showhelp(argv[0]);
	for(i = 1; i < argc; i++) {
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
                        	encparms.maxBitr -= encparms.minBitr % 8;
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
	input = fopen(argv[i], "rb");
	if (!input) {
		printf("Cannot open file: %s\n", argv[i]);
		exit(-1);
	}

	output_idx = fopen("monster.idx", "wb");
	output_snd = fopen("monster.mp3", "wb");

	/* Get the 'SOU ....' header */
	get_string(8);
	if (strncmp(buf, f_hdr, 8)) {
		printf("Bad SOU\n");
		exit(-1);
	}
	while (1)
		get_part();
	return 0;
}
