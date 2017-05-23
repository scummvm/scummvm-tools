#include <common/scummsys.h>
#include <common/file.h>


class ModReader {
public:
	ModReader(const Common::Filename&);
	~ModReader();

	bool convertToMsn(const Common::Filename& fileName, int version = 1);

private:
	Common::File modFile;

	char songName[20];         // liedname

	struct {
		char iname[22];
		uint16 length;          // laenge
		char finetune;
		char volume;            // lautst
		uint16 loopStart;       // wdh_start
		uint16 loopLength;      // wdh_laenge
	} instr[31];

	char songLength;            // liedlaenge
	char ciaaSpeed;
	char arrangement[128];
	char mark[4];               // markierung

	int16 patternNumber;        // patternzahl
	int32 note[28][64][4];

	char convInstr[31];         // konv_instr
};

