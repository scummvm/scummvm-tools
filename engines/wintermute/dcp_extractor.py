#! /usr/bin/env python3

# Sources:
# https://archive.softwareheritage.org/browse/content/sha1_git:0bc8340ae58e9d88a23e52b0723b9a92e14f4e62/?origin_url=https://bitbucket.org/MnemonicWME/wme1&path=src/engine_core/wme_base/dcpackage.h
# https://archive.softwareheritage.org/browse/content/sha1_git:4fd95c6076f4f2ce5dbbd1eead41b357bd232c66/?origin_url=https://bitbucket.org/MnemonicWME/wme1&path=src/engine_core/wme_base/BFileManager.cpp

import collections
from datetime import datetime
import functools
import pathlib
import struct
import zlib

Header = collections.namedtuple('Header', ['magic1', 'magic2', 'pkg_version', 'game_version', 'priority', 'cd', 'master_index', 'creation_time', 'desc', 'num_dirs'])
DirEntry = collections.namedtuple('DirEntry', ['name', 'cd', 'num_entries', 'files'])
FileEntry = collections.namedtuple('FileEntry', ['name', 'offset', 'length', 'comp_length', 'flags', 'timedate1', 'timedate2'], defaults=(0, 0))

def safe_decode(b):
    for enc in ("cp1252", "latin-1", "utf-8"):
        try:
            return b.decode(enc)
        except UnicodeDecodeError:
            continue
    return b.decode("latin-1", errors="replace")

def read_struct(f, fmt, constructor):
    if type(fmt) is str:
        fmt = struct.Struct(fmt)

    buf = f.read(fmt.size)
    if len(buf) != fmt.size:
        raise Exception("File too small")

    return constructor(*fmt.unpack(buf))

def read_str(f):
    sz = f.read(1)
    if len(sz) != 1:
        print("End of file!")
        return ""
    sz, = struct.unpack('<B', sz)
    s = f.read(sz)
    if len(s) != sz:
        raise Exception("File too small")
    return s

def read_headers(f, abs_offset = 0):
    f.seek(abs_offset)

    header = read_struct(f, '<L4sLLBBBxL100sL', Header)
    if header.magic1 != 0xdec0adde:
        raise Exception("Invalid magic")
    if header.magic2 != b'JUNK':
        raise Exception("Invalid magic")
    if header.pkg_version > 0x200:
        raise Exception("Invalid version")

    if header.pkg_version == 0x200:
        dir_offset, = struct.unpack('<L', f.read(4))
        dir_offset += abs_offset
        f.seek(dir_offset)

    dirs = []
    for pkg in range(header.num_dirs):
        files = []
        dir_name = read_str(f)
        dir_name = dir_name.rstrip(b'\x00')
        dirent = read_struct(f, '<BL', functools.partial(DirEntry, dir_name, files=files))
        dirs.append(dirent)

        for i in range(dirent.num_entries):
            fname = read_str(f)
            if (fname == ""):
                break
            fname = bytes(b ^ 0x44 for b in fname)
            fname = fname.rstrip(b'\x00')

            if header.pkg_version == 0x200:
                fmt = '<LLLLLL'
            else:
                fmt = '<LLLL'
            fileent = read_struct(f, fmt, functools.partial(FileEntry, fname))
            fileent = fileent._replace(offset=fileent.offset + abs_offset)
            files.append(fileent)

    return header, dirs

def read_file(f, fileent):
    f.seek(fileent.offset)
    if fileent.comp_length:
        buf = f.read(fileent.comp_length)
        buf = zlib.decompress(buf)
    else:
        buf = f.read(fileent.length)
    if len(buf) != fileent.length:
        raise Exception("Invalid file size")
    return buf

def lookup_sig(f):
    import mmap
    with mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ) as mm:
        offset = mm.find(b'\xde\xad\xc0\xdeJUNK')
    if offset == -1:
        raise Exception("Signature not found")
    return offset

def dcp_list(options, offset=0):
    header, dirs = read_headers(options.input, offset)
    for dirent in dirs:
        print("Directory {0} from CD {1} with {2} entries".format(dirent.name.decode('utf-8'), dirent.cd, dirent.num_entries))
        print("{0:<8}\t@{1:<8}\t{2:<8}\t{3:<19}\t{4}".format('sz', 'offset  ', 'compsz', 'date', 'name'))
        for fl in dirent.files:
            print("{0:<8}\t@{1:<8}\t{2:<8}\t{3}\t{4}".format(
                fl.length, fl.offset, fl.length if fl.comp_length == 0 else fl.comp_length,
                datetime.fromtimestamp(fl.timedate1 | (fl.timedate2 << 64)).isoformat(), safe_decode(fl.name)))

def dcp_extract(options, offset=0):
    header, dirs = read_headers(options.input, offset)

    output_dir = options.output_dir
    for dirent in dirs:
        print("Directory {0} from CD {1} with {2} entries".format(dirent.name.decode('utf-8'), dirent.cd, dirent.num_entries))
        print("{0:<8}\t@{1:<8}\t{2:<8}\t{3:<19}\t{4}".format('sz', 'offset  ', 'compsz', 'date', 'name'))
        output_int = output_dir / dirent.name.decode('utf-8')
        for fl in dirent.files:
            print("{0:<8}\t@{1:<8}\t{2:<8}\t{3}\t{4}".format(
                fl.length, fl.offset, fl.length if fl.comp_length == 0 else fl.comp_length,
                datetime.fromtimestamp(fl.timedate1 | (fl.timedate2 << 64)).isoformat(), safe_decode(fl.name)))
            output_file = output_int / pathlib.Path(safe_decode(fl.name).replace('\\', '/'))
            output_file.parent.mkdir(parents=True, exist_ok=True)

            with output_file.open('wb') as output_f:
                buf = read_file(options.input, fl)
                output_f.write(buf)

def main():
    import argparse

    parser = argparse.ArgumentParser(
        prog='dcp_extractor.py',
        description='Wintermute DCP archive extractor')

    parser.add_argument('--sfx', action='store_true')
    parser.add_argument('input', type=argparse.FileType('rb'), metavar='dcp file')
    parser.add_argument('output_dir', nargs='?', type=pathlib.Path,
                        default=pathlib.Path('output'), metavar='output directory',
                        help='Output directory (default: ./output)')
    parser.set_defaults(action=dcp_extract)

    options = parser.parse_args()

    offset = 0
    if options.sfx:
        offset = lookup_sig(options.input)
        print("Found signature at {}".format(offset))

    options.action(options, offset=offset)

if __name__ == "__main__":
    main()
