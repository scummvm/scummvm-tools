#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
//#include <unistd.h>

FILE *input, *output_idx, *output_snd;

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

void put_int(unsigned int val) ;

void end_of_file() {
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
  
  exit(-1);
}

void get_string(int size) {
  int i = 0;
  while (i < size) {
    int c = fgetc(input);
    if (c == EOF) end_of_file();
    buf[i++] = c;
  }
  buf[i] = '\0';
}

int get_int(int size) {
  int ret = 0;
  while (size > 0) {
    int c = fgetc(input);
    if (c == EOF) end_of_file();
    ret <<= 8;
    ret |= c;
    size--;
  }
  return ret;
}
void append_byte(int size) {
  int i;
  int c;
  for (i = 0; i < (size - 1); i++) buf[i] = buf[i + 1];
  c = fgetc(input);
  if (c == EOF) end_of_file();
  buf[i] = c;
}

void put_int(unsigned int val) {
  int i;
  for (i = 0; i < 4; i++) {
    fputc(val >> 24, output_idx);
    val <<= 8;
  }
}

void get_part() {
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
  if (tags < 0) exit(-1);
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
  case 0x01:  {
    int length = 0;
    int i;
    int sample_rate;
    int comp;
    FILE *f;
    char fbuf[2048];
    char fbuf_o[4096];
    int size;
    int tot_size;
    int real_samplerate;
    char rawname[256];
    char mp3name[256];
    static int sound_num = 0;
    
    /* Sound Data */
    printf(" Sound Data\n");
    for (i = 0; i < 3; i++) length = length | (fgetc(input) << (i * 8));
    printf(" - length = %d\n", length);
    sample_rate = fgetc(input);
    comp = fgetc(input);
    real_samplerate = 1000000 / (256 - sample_rate);
    printf(" - sample rate = %d (%02x)\n", 1000000 / (256 - sample_rate), sample_rate);
    printf(" - compression = %s (%02x)\n", (comp == 0 ? "8bits" :
					    (comp == 1 ? "4bits" :
					     (comp == 2 ? "2.6bits" :
					      (comp == 3 ? "2bits" : "Multi")))), comp);

    /* real_samplerate = (sample_rate == 0xd5 ? 22050 :
       (sample_rate == 0xd3 ? 22050 :
       (sample_rate == 0xd2 ? 22050 :
       (sample_rate == 0xa5 ? 11025 :
       (sample_rate == 0xa6 ? 11025 :
       (sample_rate == 0x83 ? 8000 : 
       -1)))))); */
    if (comp != 0) {
      exit(-1);
    }
#if 1
#ifdef DEBUG
    sprintf(rawname, "tempfile_%05d.raw", sound_num);
    sprintf(mp3name, "tempfile_%05d.mp3", sound_num++);
#else
    sprintf(rawname, "tempfile.raw");
    sprintf(mp3name, "tempfile.mp3");
#endif
    f = fopen(rawname, "wb");
    length -= 2;
    while (length > 0) {
      size = fread(fbuf, 1, length > 2048 ? 2048 : length, input);
      if (size <= 0) break;
      length -= size;
      for (i = 0; i < size; i++) {
	fbuf_o[2 * i    ] = fbuf[i] ^ 0x80;
	fbuf_o[2 * i + 1] = 0;
      }
      fwrite(fbuf_o, 1, 2 * size, f);
    }
    fclose(f);
#else
    fseek(input, length - 2, SEEK_CUR);
#endif
    
#if 1
    sprintf(fbuf, "lame -h -t -q 0 --vbr-new -V 9 -b 24 -B 32 --resample 22.05 -m m --bitwidth 16 -r -s %d %s %s", real_samplerate, rawname, mp3name);
    system(fbuf);
    
    f = fopen(mp3name, "rb");
    tot_size = 0;
    while ((size = fread(fbuf, 1, 2048, f)) > 0) {
      tot_size += size;
      fwrite(fbuf, 1, size, output_snd);
    }
    fclose(f);
    put_int(tot_size);
#endif
  } break;

  default:
    printf("Unknown chunk : %02x\n", id);
    exit(-1);
    break;
  }
}

int main(int argc, char *argv[]) {
  input = fopen(argv[1], "rb");
  if (!input) {
   printf("Cannot open file: %s\n", argv[1]);
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

  fclose(output_idx);
  fclose(output_snd);
  fclose(input);

  /* And the final concatenation */
  
  return 0;
}
