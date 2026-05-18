// Compiled by Borland BC5.02 (BCC32.EXE -O2)
// (c) Alexander Trush http://trush.da.ru  mailto:trush@kbotd.ru
//
// Tab stops is 8
//
// 02/02/00 v1.0
// 03/02/00 v1.1 autosearch of header of the disk
// 07/02/00 v1.2 now header of disk is zero sector, EOF control


#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>


#define BUFFERLEN 102400  // Buffer size

FILE *log;

static inline unsigned int endianswap(unsigned int x) {

        // For BigEndians machines simple return x;
	return (x>>24) | ((x&0x00FF0000L)>>8) | ((x&0x0000FF00L)<<8) | (x<<24);

}


struct DirHeadType {
	unsigned int CurPageNum;
	unsigned int unknow1;
	unsigned int unknow2;
	unsigned int SizeOfDir;
	unsigned int unknow3;
} DirHead;

struct FileEntryType {
	unsigned int Attr;
	unsigned int unknow1;  // Possible hash of rest of this sctruct???
	char FileExt[4];
	unsigned int unknow2;  // Size of Blocks?
	unsigned int FileSizeBytes;
	unsigned int FileSizeBlocks;
	unsigned int unknow3;
	unsigned int unknow4;
	char FileName[32];
	unsigned int NumberCopies;
	unsigned int StartBlock;
} FileEntry;



unsigned int SizeOfBlock;


void ExpandDir(FILE *infile, unsigned int offset, unsigned int block) {

   char s[5]="Extn";
   unsigned int Attr;
   unsigned int pos;

   fprintf(log,"\n__BEGIN__\n");

   do
     {
       fseek(infile, offset+block*SizeOfBlock, SEEK_SET);
       if( fread(&DirHead, sizeof(DirHead), 1, infile)!=1 )
         {
	    fprintf(log,"\nCan't read DirHeader\n");
	    printf("\nCan't read DirHeader\n");
            return;
         }
       do
	 {
	 unsigned int len;

	    if( fread(&FileEntry, sizeof(FileEntry), 1, infile)!=1 )
	      {
                fprintf(log,"\nCan't read FileEntry\n");
                printf("\nCan't read FileEntry\n");
                return;
              }
	    memcpy(s, FileEntry.FileExt, 4);
	    Attr=endianswap(FileEntry.Attr);
	    fprintf(log,"[%08X ",  Attr);
	    fprintf(log,"%08X ",  endianswap(FileEntry.unknow1));
	    fprintf(log,"0x%X ",   endianswap(FileEntry.unknow2));
	    fprintf(log,"%X ",   endianswap(FileEntry.unknow3));
	    fprintf(log,"%X] ", endianswap(FileEntry.unknow4));
	    fprintf(log,"\tPosInFile:%8X  ",offset+SizeOfBlock*endianswap(FileEntry.StartBlock));
	    fprintf(log,"Size: %8X ",   endianswap(FileEntry.FileSizeBytes));
	    fprintf(log,"\"%s\", filename: %s\n", s, FileEntry.FileName);

	    printf("\tFile: %-32s ", FileEntry.FileName);

	    len=endianswap(FileEntry.FileSizeBytes);
	    if(len<(1<<9)) printf("Len:%5d\t\r", len);
	    else if(len<(1<<19)) printf("Len:%6.2f Kbytes\t\r", (float)len/(1<<10));
	    	 else printf("Len:%7.2f Mbytes\t\r", (float)len/(1<<20));

	    if( (pos=endianswap(FileEntry.NumberCopies))>0)
	      {
		 int t;
		   fprintf(log,"%d copies ignoreg. ", pos);
		   while(pos--) fread(&t, 4, 1, infile); //Skip backup pos.
	      }

	    pos = ftell(infile);  // Cut under IF

	    if(endianswap(FileEntry.NumberCopies)>0)
		 fprintf(log,"Position aftes skip copies is 0x%X\n", pos);

	    fflush(log);

	    if( (Attr & 255) == 7)
	      {
		 mkdir(FileEntry.FileName, 0775); chdir(FileEntry.FileName);
		 ExpandDir(infile, offset, endianswap(FileEntry.StartBlock));
		 fseek(infile, pos, SEEK_SET);
		 chdir("..");
	      }
	    if( (Attr & 255) == 2  ||  (Attr & 255) == 6 )
	      {
		 FILE *f;
		 float sizeoffile=endianswap(FileEntry.FileSizeBytes);

		   fseek(infile, offset+SizeOfBlock*endianswap(FileEntry.StartBlock), SEEK_SET);
		   f=fopen(FileEntry.FileName,"wb");

		   while(len>=BUFFERLEN)
		     {
		        char sector_buf[BUFFERLEN];

			   printf("%5.1f%%\r", 100.0*(1.0-len/sizeoffile));

			    if( fread(&sector_buf, BUFFERLEN, 1, infile)!=1 )
			      {
		        l1:     fprintf(log,"Can't read file \"%s\"\n\n", FileEntry.FileName);
		                printf("\nCan't read contents of file \"%s\", possible image file broken\n", FileEntry.FileName);
                                fclose(f);
				unlink(FileEntry.FileName);
		                goto l2;
		              }
                           fwrite(&sector_buf, BUFFERLEN, 1, f);
                           len-=BUFFERLEN;
                     }
		   while(len--)
                     {
                       if(feof(infile)) goto l1;
                       fputc(fgetc(infile), f);
                     }
                   fclose(f);
		l2:fseek(infile, pos, SEEK_SET);
	      }

	 } while(Attr < 256L);

       fprintf(log,"Attr:%X PosNext: %X\n", Attr, pos);
       block +=2048/SizeOfBlock;

     } while((Attr & 0xFF000000L) == 0x40000000L);

   fprintf(log,"\n___END__\n\n\n\n");

}


int TestHeader(FILE *infile, unsigned int offset, unsigned int block)
{
    char buf[8];

	printf("Test of sector: %d\r", block);
	fseek(infile, offset+block*SizeOfBlock, SEEK_SET);
	fread(&buf, 8,  1, infile);
	if(strncmp(buf, "\1ZZZZZ\1", 7) != 0) return 0;
	fseek(infile, offset+block*SizeOfBlock+0x28, SEEK_SET);
	fread(&buf, 7,  1, infile);
	if(strncmp(buf, "CD-ROM", 6) != 0) return 0;
	return 1;
}


/**************************************************************************/
#pragma argsused
int main(int argc, char **argv)
{

  FILE *infile;
  char DirName[1024]="CDROM_";


	if(argc<2)
	  {
             printf(	"\n\tunCD-ROM v1.2 by Alexander Trush (http://3do.da.ru)\n"
			"\tBase on program of Hitoshi Kashima (http://www.keisei.tsukuba.ac.jp/~kashima)\n\n"
			"\tUsage:  unCD-ROM.exe GAME_CD.img\n\n"
			"\t\twhere GAME_CD.img - MODE1/2048 3DO CD-ROM image file\n");
             return 1;
	  }

        log=fopen("unCD-ROM.log","at");
        infile=fopen(argv[1], "rb");
	if(infile)
	  {
             unsigned int t, maxblocks, offset;
	     char *p;


		offset=0;
		printf("\nLooking for 3DO CD-ROM disc header in image: \"%s\"\n\n", argv[1]);
                SizeOfBlock=2048;
		fseek(infile, offset, SEEK_END);
		maxblocks= ftell(infile)/SizeOfBlock;  // Get file size
		for(t=0; t<maxblocks; t++) if (TestHeader(infile, offset, t)) break;

		if(TestHeader(infile, offset, t))
		  {
			offset=t*SizeOfBlock; // Now header of disk is zero sector
			fprintf(log,"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
				    "Disk image: \"%s\"\n"
				    "Found CD-ROM header at sector #%d (offset 0x%x)\n\n", argv[1], t, offset);
			fflush(log);

			p = strrchr(argv[1], (int)'.');
			if(p!=NULL) *p='\0';

			strcpy(DirName+strlen(DirName), argv[1]);
			fseek(infile, offset+0x64L, SEEK_SET);
			fread(&t, 4, 1, infile); t = endianswap(t);
			printf("\nMake dir: \"%s\"\n\n", DirName);
			mkdir(DirName, 0775);
			chdir(DirName);
			ExpandDir(infile, offset, t);
			chdir("..");
			printf("Done!\t\t\t\t\t\t\t\t\t\n");
			fprintf(log,"Done!\n\n\n\n\n\n\n\n\n");
		  }
		else
		  {
	             printf("\n\tFile: \"%s\" is not MODE1/2048 3DO CD-ROM images\n", argv[1]);
	             return 1;
		  }

	  }
	else
	  {
             printf("\n\tCan't open 3DO CD-ROM image file:\"%s\"\n", argv[1]);
             return 1;
	  }


	fclose(infile);
	return 0;
}
