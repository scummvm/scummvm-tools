#ifndef COMMON_FILE_H
#define COMMON_FILE_H

#include "stream.h"

namespace Common {

class File : public SeekableReadStream {
protected:
	/** POSIX file handle to the actual file; 0 if no file is open. */
	FILE *_handle;

	/** Status flag which tells about recent I/O failures. */
	bool _ioFailed;

private:
	// Disallow copying File objects. There is not strict reason for this,
	// except that so far we never had real need for such a feature, and
	// code that accidentally copied File objects tended to break in strange
	// ways.
	File(const File &f);
	File &operator  =(const File &f);

public:

	File(FILE*file);
	virtual ~File();

	virtual void close();
	bool isOpen() const;
	bool ioFailed() const;
	void clearIOFailed();
	bool eos() const { return eof(); }
	bool eof() const;
	uint32 pos() const;
	uint32 size() const;
	void seek(int32 offs, int whence = SEEK_SET);
	uint32 read(void *dataPtr, uint32 dataSize);
};

} // End of namespace Common

#endif
