#! /bin/sh -e

dest=loom-pce
cleanup=1
compress=0
ext=wav

# This is a proof-of-concept script. I'm at best a bumbling amateur
# when it comes to shell scripting. # Use this script from an empty
# directory. Required packages (Debian):
#
# - cdrdao
# - bchunk
# - flac (optional)

while [ $# -gt 0 ] ; do
    case $1 in
	--help)
	    echo "Usage: $0 [OPTION] [DEST]"
	    echo "    --help      This text"
	    echo "    --keep      Do not remove temporary files"
	    echo "    --compress  Compress audio tracks to FLAC"
	    exit 0
	    ;;
	--keep)
	    cleanup=0
	    ;;
	--compress)
	    compress=1
	    ;;
	--)
	    shift
	    break
	    ;;
	-*)
	    echo $0: $1: unrecognized option >&2
	    exit 1
	    ;;
	*)
	    break
	    ;;
    esac

    shift
done

if [ $# -gt 0 ] ; then
    dest=$1
fi

# Create an new directory for the final game. Make sure that it's empty

mkdir -p "$dest"

if [ -n "`ls -A \"$dest\"`" ] ; then
    echo "$0: Destination directory is not empty: $dest"
    exit 1
fi

# Rip the CD to hard disk. The cdrdao command has a bewildering number
# of options, but apparently none for extracting single tracks. Oh
# well, most of the tracks are interesting in some way.
#
# After some experimenting I came up with pretty much the same
# parameters as in the DOSBox documentation, at which point I decided
# to just follow their lead. The most interesting bit is probably the
# 0x20000 option, which ensures the correct byte order for the
# extracted audio. This allows the resulting BIN/CUE files to be run
# in the Mednafen emulator, which is useful for testing.
#
# It's also possible to do this byte swapping in the bchunk step, by
# adding the -s option there.
#
# This step is time-consuming, so only do it if there isn't already an
# extracted loom.bin file there. It may be possible to generate a CUE
# file instead of a TOC file here, but I didn't manage to.

if [ ! -e loom.bin ] ; then
    cdrdao read-cd --datafile loom.bin --driver generic-mmc:0x20000 --device /dev/cdrom --read-raw loom.toc
fi

# If there isn't a CUE file, convert the TOC file into something other
# programs can work with.

if [ ! -e loom.cue ] ; then
    toc2cue loom.toc loom.cue
fi

# Split the BIN/CUE files into individual tracks.

bchunk -w loom.bin loom.cue track

# Compress the audio tracks. Note that there is a gap in the numbering
# because of the data track.

if [ $compress != 0 ] ; then
    flac -8 *.wav
    ext=flac
fi
 
mv track01.$ext "$dest"

prevnr=02

for nr in `seq -w 3 22` ; do
    mv "track$nr.$ext" "$dest/track$prevnr.$ext"
    prevnr=$nr
done

# Finally, use scummvm-tools-cli to extract the oh-so-juicy data files.

scummvm-tools-cli --tool extract_loom_tg16 -o "$dest" track02.iso

# Cleanup

if [ $cleanup != 0 ] ; then
    rm -f loom.toc track02.iso track23.iso track??.wav
fi

