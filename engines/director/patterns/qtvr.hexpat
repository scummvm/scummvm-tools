// inspired by: https://gist.github.com/Zaggy1024/a030cf23069ed62d452161f579ed272f

#pragma endian big


#include <std/core.pat>
#include <std/fxpt.pat>
#include <std/io.pat>
#include <std/math.pat>
#include <std/mem.pat>
#include <std/string.pat>
#include <std/time.pat>
#include <type/byte.pat>
#include <type/guid.pat>
#include <type/size.pat>

#define MIN_BOX_SIZE 8

fn contents_size(u128 base_offset, u64 size) {
    s128 end = size == 0 ? std::mem::size() : base_offset + size;
    end = std::math::min(end, std::mem::size());
    s128 offset_signed = $;
    return end - offset_signed;
};


struct Atom {
    type::Size32 box_size;
    char box_type[4];

    if (box_size == 1) {
        type::Size64 box_size;
    }
};

struct FullAtom : Atom {
    u8 version;
    u24 flags;
};

fn to_float(auto value) {
    return std::string::parse_float(std::string::to_string(value));
};
fn format_fixed(u32 value, u32 precision) {
    // double cast is broken it seems
	return to_float(value) + to_float(1 << precision);
};

fn format_fixed_signed(s32 value, u32 precision) {
    float val = value >> precision;
    s32 second_val = value & ( 1 << precision - 1);
    float comma = second_val / ( 1 << precision + 1);
    return val + comma;
};

using fixed_s32 = s32 [[format("format_fixed_signed_32")]];
using fixed_u32 = u32 [[format("format_fixed_32")]];
fn format_fixed_signed_32(s32 value) {
	return format_fixed_signed(value, 16);
};
fn format_fixed_32(s32 value) {
	return format_fixed(value, 16);
};

using fixed_s16 = s16 [[format("format_fixed_16")]];
fn format_fixed_16(u16 value) {
	return format_fixed(value, 8);
};
using normal_s32 = s32 [[format("format_normal_32")]];
fn format_normal_32(auto value) {
    return format_fixed(value, 30);
};

struct Matrix {
    fixed_s32 matrix_00;
    fixed_s32 matrix_01;
    fixed_s32 matrix_02;
    fixed_s32 matrix_10;
    fixed_s32 matrix_11;
    fixed_s32 matrix_12;
    normal_s32 matrix_20;
    normal_s32 matrix_21;
    normal_s32 matrix_22;
} [[format("format_matrix")]];

fn format_matrix(auto matrix) {
    return std::format("[ [ {}, {}, {} ], [ {}, {}, {} ], [ {}, {}, {} ] ]",
        format_fixed_32(matrix.matrix_00), format_fixed_32(matrix.matrix_01), format_fixed_32(matrix.matrix_02),
        format_fixed_32(matrix.matrix_10), format_fixed_32(matrix.matrix_11), format_fixed_32(matrix.matrix_12),
        format_normal_32(matrix.matrix_20), format_normal_32(matrix.matrix_21), format_normal_32(matrix.matrix_22));
};

using UnknownAtom;

struct CTYPAtom : Atom {
    char id[4];
};

struct NAVGAtom: Atom {
    u16 version;
    u16 columns;
    u16 rows;
    padding[2];
    u16 loop_size;
    u16 frame_duration;

    u16 movie_type;
    u16 ticks;
    fixed_s32 field_of_view;
    fixed_s32 startHPan;
    fixed_s32 endHPan;
    fixed_s32 endVPan;
    fixed_s32 startVPan;
    fixed_s32 initialHPan;
    fixed_s32 initialVPan;
    padding[4];

};

// moov -> udta
struct UserDataChildAtomSelector {
    Atom atom_header [[hidden]];

    $ = addressof(this);

    match(atom_header.box_type) {
        ("ctyp"): CTYPAtom ctyp;
        ("NAVG"): NAVGAtom navg;
        (_): UnknownAtom unknown [[name(atom_header.box_type)]];
    }
} [[inline]];


struct UserDataAtom : Atom {
    UserDataChildAtomSelector children[while (contents_size(addressof(this), box_size) > MIN_BOX_SIZE)];

    // handle optional padding
    // ToDo? improve it?
    u128 current_pos = $;
    u32 trailingZero @ $;
    $ = current_pos;

    $ += contents_size(addressof(this), box_size);
};


fn appears_to_contain_boxes(ref u128 contents_offset, u128 contents_size) {
    u128 current_offset = contents_offset;
    while (true) {
        u32 current_size_raw @ current_offset;
        u32 current_size = be u32(current_size_raw);
        current_offset += current_size;
        if (current_offset == contents_offset || current_offset >= (contents_offset + contents_size)) {
            break;
        }
    }
    return current_offset == (contents_offset + contents_size);
};

struct UnknownAtomSelector {
    Atom atom_header [[hidden]];
    $ = addressof(this);
    UnknownAtom unknown [[name(atom_header.box_type)]];
    std::print("unkown: " + atom_header.box_type);
} [[inline]];

struct UnknownAtom : Atom {
    if (appears_to_contain_boxes($, contents_size(addressof(this), box_size))) {
        UnknownAtomSelector children[while (contents_size(addressof(this), box_size) > MIN_BOX_SIZE && std::mem::size() - $ > MIN_BOX_SIZE)];
    } else {
        $ += sizeof(u32);
        if (appears_to_contain_boxes($, contents_size(addressof(this), box_size))) {
            $ -= sizeof(u32);
            u8 version;
            u24 flags;
            UnknownAtomSelector children[while (contents_size(addressof(this), box_size) > MIN_BOX_SIZE && std::mem::size() - $ > MIN_BOX_SIZE)];
        } else {
            $ -= sizeof(u32);
        }
    }
    u128 next_offset = std::math::min($ + contents_size(addressof(this), box_size), std::mem::size());
    if (next_offset > $) {
        u8 unknown_data[next_offset - $] [[sealed]];
    } else {
        $ = next_offset;
    }
    std::print("unkown atom: {}: {}", box_type, $);
} [[format("format_box_type")]];

fn format_box_type(auto box) {
    return box.box_type;
};

// original has formatting
// original parses times as datetimes

struct MovieHeaderAtom : FullAtom {
    u32 creation_time;
    u32 modification_time;
    u32 timescale;
    u32 duration;
    fixed_s32 rate;
    fixed_s16 volume;
    padding[10];
    Matrix matrix;
    u32 preview_time;
    u32 preview_duration;
    u32 poster_time;
    u32 selection_time;
    u32 selection_duration;
    u32 current_time;
    u32 next_track_id;
}[[format("format_mvhd")]];

bitfield TrackHeaderFlags {
    padding : 4;
    bool track_in_poster: 1;
    bool track_in_preview: 1;
    bool track_in_movie: 1;
    bool track_enabled : 1;
};

struct TrackHeaderAtom : Atom {
    u8 version;
    padding[2];
    TrackHeaderFlags;
    u32 creation_time;
    u32 modification_time;
    u32 track_id;
    padding[sizeof(u32)];
    u32 duration;
    padding[sizeof(u32) * 2];
    s16 layer;
    u16 alternate_group;
    fixed_s16 volume;
    padding[sizeof(u16)];
    Matrix matrix;
    fixed_s32 track_width;
    fixed_s32 track_height;
};

using EditListAtom;

struct EditAtom : Atom {
    EditListAtom edits[while (contents_size(addressof(this), box_size) > MIN_BOX_SIZE)];
    $ += contents_size(addressof(this), box_size);
};

// moov -> trak -> edts -> elst
struct EditListEntry<DurationT, TimeT> {
    DurationT segment_duration;
    TimeT media_time;
    s16 media_rate_integer;
    s16 media_rate_fraction;
};

struct EditListAtom : FullAtom {
    u32 entry_count;
    EditListEntry<u32, s32> entries[entry_count];
    $ += contents_size(addressof(this), box_size);
};

using MediaHeaderAtom;
using HandlerAtom;

using SampleTableChildAtomSelector;

struct SampleTableAtom : Atom {
    SampleTableChildAtomSelector children[while(contents_size(addressof(this), box_size) > 0)];
};

struct DataPointer32 {
    u8* pointer : u32;
} [[inline]];

struct ChunkOffsetAtom : FullAtom {
    u32 entry_count;
    DataPointer32 chunk_offset[entry_count];
    $ += contents_size(addressof(this), box_size);
};

struct UnknownSampleEntryChildSelector {
    Atom box_header [[hidden]];

    $ = addressof(this);
    match(atom_header.box_type) {
        (_): UnknownAtom unknown [[name(box_header.box_type)]];
    }
} [[inline]];

struct SampleEntry : Atom {
    padding[sizeof(u8) * 6];
    u16 data_reference_index;
};

struct UnknownSampleEntry : SampleEntry {
    if (appears_to_contain_boxes($, contents_size(addressof(this), box_size))) {
        UnknownSampleEntryChildSelector children[while (contents_size(addressof(this), box_size) > MIN_BOX_SIZE)];
    }
    $ += contents_size(addressof(this), box_size);
};

enum VisualSampleDepth : u16 {
    ColourNoAlpha = 0x0018,
};

struct VisualSampleEntry : SampleEntry {
    u16 version;
    u16 revision;
    char vendor[4];
    u32 temporal_quality;
    u32 spatial_quality;
    u16 width;
    u16 height;
    fixed_s32 horizresolution; // in pixels per inch
    fixed_s32 vertresolution; // in pixels per inch
    padding[sizeof(u32)];
    u16 frame_count_per_sample;
    u8 compressorname_size [[hidden]];
    char compressorname[compressorname_size];
    $ = addressof(compressorname_size) + 32;
    u16 depth;
    s16 colorTableID;
    if (colorTableID != -1 ) {
        std::print("Special color table");
    }
    $ += contents_size(addressof(this), box_size);
};

struct PanoSampleDescription: Atom {
    // https://developer.apple.com/library/archive/technotes/tn/tn1035.html
    std::print("pano located at: 0x{:X}", addressof(this));
    padding[sizeof(u32)];
    padding[sizeof(u32)]; // must be zero, sometimes it contains a 1

    s16 majorVersion; // must be zero, sometimes it contains a 1
    s16 minorVersion;
    s32 sceneTrackID;
    s32 loResSceneTrackID;
    padding[sizeof(u32) * 6];
    s32 hotspotTrackID;
    padding[sizeof(u32) * 9];
    fixed_s32 hPanStart;
    fixed_s32 hPanEnd;
    fixed_s32 vPanTop;
    fixed_s32 vPanBottom; // TODO: number is off...
    fixed_s32 minimumZoom;
    fixed_s32 maximumZoom;

    // Info for highest res version of scene track

    s32 sceneSizeX;
    s32 sceneSizeY;
    s32 numFrames;
    padding[sizeof(u16)];
    s16 sceneNumFramesX;
    s16 sceneNumFramesY;
    s16 sceneColorDepth;

    // Info for highest res version of hotSpot track
    s32 hotSpotSizeX;
    s32 hotSpotSizeY;
    padding[sizeof(u16)];
    s16 hotSpotNumFramesX;
    s16 hotSpotNumFrameY;
    s16 hotspotColorDepth;

    $ += contents_size(addressof(this), box_size);
};

struct SampleEntrySelector {
    Atom atom_header [[hidden]];

    $ = addressof(this);
    match(atom_header.box_type) {
        ("cvid"): VisualSampleEntry cvid;
        ("smc "): VisualSampleEntry smc;
        ("rle "): VisualSampleEntry rle;
        ("pano"): PanoSampleDescription pano;
        ("SVQ1"): VisualSampleEntry svq;
        //("qtvr"): QTVRSampleDescription qtvr;
        (_):  UnknownSampleEntry unknown [[name(atom_header.box_type)]];
    }

} [[inline]];

struct SampleDescriptionAtom : FullAtom {
    u32 entry_count;
    SampleEntrySelector children[entry_count];
    $ += contents_size(addressof(this), box_size);
};



// moov -> trak -> mdia -> minf -> stbl -> stts
struct TimeToSampleEntry {
    u32 sample_count;
    u32 sample_duration;
} [[format("format_stts_entry")]];

fn format_stts_entry(auto stts_entry) {
    return std::format("count = {}, duration = {}", stts_entry.sample_count, stts_entry.sample_duration);
};

struct TimeToSampleAtom : FullAtom {
    u32 entry_count;
    TimeToSampleEntry entries[entry_count] [[format("count_samples")]];
    $ += contents_size(addressof(this), box_size);
};

fn count_samples(auto entries) {
    u128 count = 0;
    for (u32 i = 0, i < std::core::member_count(entries), i += 1) {
        count += entries[i].sample_count;
    }
    return std::format("{} samples total", count);
};

// moov -> trak -> mdia -> minf -> stbl -> stsc
struct SampleToChunkAtomEntry {
    u32 first_chunk;
    u32 samples_per_chunk;
    u32 sample_description_index;
} [[format("format_stsc_entry")]];

fn format_stsc_entry(auto stsc_entry) {
    return std::format("first_chunk = {}, samples_per_chunk = {}, sample_description_index = {}",
        stsc_entry.first_chunk, stsc_entry.samples_per_chunk, stsc_entry.sample_description_index);
};

struct SampleToChunkAtom : FullAtom {
    u32 entry_count;
    SampleToChunkAtomEntry entries[entry_count];
    $ += contents_size(addressof(this), box_size);
};

using SampleSizeAtom;
struct SampleTableChildAtomSelector {
    Atom atom_header [[hidden]];

    $ = addressof(this);

    match(atom_header.box_type) {
        ("stsd"): SampleDescriptionAtom stsd;
        ("stsz"): SampleSizeAtom stsz;
        ("stco"): ChunkOffsetAtom stco;
        ("stts"): TimeToSampleAtom stts;
        ("stsc"): SampleToChunkAtom stsc;
        (_): UnknownAtom unknown [[name(atom_header.box_type)]];
    }

} [[inline]];

struct SampleSizeAtom : FullAtom {
    u32 sample_size;
    u32 sample_count;
    if (sample_size == 0) {
        u32 entry_size[sample_count];
    }
    $ += contents_size(addressof(this), box_size);
};

struct VideoMediaHeaderAtom : FullAtom {
    u16 graphicsmode;
    u16 opcolor[3];
    $ += contents_size(addressof(this), box_size);
};


// moov -> trak -> mdia -> minf -> dinf
using DataReferenceAtom;

struct DataEntryAtomSelector {
    Atom atom_header [[hidden]];

    $ = addressof(this);
    match(atom_header.box_type) {
        ("alis"): FullAtom alis;
        (_): UnknownAtom unknown [[name(atom_header.box_type)]];
    }

} [[inline]];

struct DataReferenceAtom : FullAtom {
    u32 entry_count;
    DataEntryAtomSelector children[entry_count];
    $ += contents_size(addressof(this), box_size);
};

struct DataInformationChildAtomSelector {
    Atom atom_header [[hidden]];

    $ = addressof(this);
    match(atom_header.box_type) {
        ("dref"): DataReferenceAtom dref;
        (_): UnknownAtom unknown [[name(atom_header.box_type)]];
    }

} [[inline]];

struct DataInformationAtom : Atom {
    DataInformationChildAtomSelector children[while (contents_size(addressof(this), box_size) > MIN_BOX_SIZE)];
};

struct BaseMediaInfoAtom: FullAtom {
    u16 graphics_mode;
    u16 opcolor_1;
    u16 opcolor_2;
    u16 opcolor_3;
    u16 balance;
    padding[sizeof(u16)];
};

struct PanoMediaEntry {
    u32 nodeID;
    u32 time;
};

struct PanoMediaInfoAtom: Atom {
    std::print("pINF: 0x{:X}", $);
    std::string::SizedString<u8> name;
    // string is padded till the 32nd byte.
    // this is the beginning of the atom, including the 8 byte.
    padding[40-($ - addressof(this))];
    u32 defNodeId;
    fixed_s32 defZoom;
    s32 reserved;
    s16 pad;
    s16 numEntries;
    PanoMediaEntry entries[numEntries];
    $ += contents_size(addressof(this), box_size);

}[[hex::visualize("hex_viewer", this)]];

struct STPanoMediaInfoChildAtomSelector {
    Atom atom_header [[hidden]];

    $ = addressof(this);
    match(atom_header.box_type) {
        ("pInf"): PanoMediaInfoAtom pInf;
        (_): UnknownAtom unknown [[name(atom_header.box_type)]];
    }
} [[inline]];

struct STPanoMediaInfoAtom: Atom {
    STPanoMediaInfoChildAtomSelector children[while (contents_size(addressof(this), box_size) > MIN_BOX_SIZE)];
};

struct GMHDChildAtomSelector {
    Atom atom_header [[hidden]];

    $ = addressof(this);

    match(atom_header.box_type) {
        ("gmin"): BaseMediaInfoAtom gmin;
        ("STpn"): STPanoMediaInfoAtom STpn;
        (_): UnknownAtom unknown [[name(atom_header.box_type)]];
    }

} [[inline]];

struct GMHDAtom: Atom {
    GMHDChildAtomSelector children[while (contents_size(addressof(this), box_size) > MIN_BOX_SIZE)];
    $ += contents_size(addressof(this), box_size);
};

struct MediaInformationChildAtomSelector {
    Atom atom_header [[hidden]];

    $ = addressof(this);

    match(atom_header.box_type) {
        ("stbl"): SampleTableAtom stbl;
        ("hdlr"): HandlerAtom hdlr;
        ("vmhd"): VideoMediaHeaderAtom vmhd;
        ("dinf"): DataInformationAtom dinf;
        ("gmhd"): GMHDAtom gmhd;
        (_): UnknownAtom unknown [[name(atom_header.box_type)]];
    }
} [[inline]];

struct MediaInformationAtom : Atom {
    MediaInformationChildAtomSelector children[while(contents_size(addressof(this), box_size) > 0)];
};

struct MediaChildAtomSelector {
    Atom atom_header [[hidden]];

    $ = addressof(this);

    match(atom_header.box_type) {
        ("mdhd"): MediaHeaderAtom mdhd;
        ("hdlr"): HandlerAtom hdlr;
        ("minf"): MediaInformationAtom minf;
        (_): UnknownAtom unknown [[name(atom_header.box_type)]];
    }

} [[inline]];

struct MediaAtom : Atom {
    MediaChildAtomSelector children[while (contents_size(addressof(this), box_size) > 0)];
};

struct TrackChildAtomSelector {
    Atom atom_header [[hidden]];

    $ = addressof(this);

    match(atom_header.box_type) {
        ("tkhd"): TrackHeaderAtom tkhd;
        ("mdia"): MediaAtom mdia;
        ("edts"): EditAtom edts;
        (_): UnknownAtom unknown [[name(atom_header.box_type)]];
    }
} [[inline]];

struct TrackAtom : Atom {
    TrackChildAtomSelector children[while(contents_size(addressof(this), box_size) > 0)];
};
str current_handler;

// (mdia|meta) -> hdlr
struct HandlerAtom : FullAtom {

    char component_type[4];
    char component_subtype[4];
    char component_manufacturer[4];
    u32 component_flags;
    u32 component_flags_mask;
    if (box_size > 32) {
        std::string::SizedString<u8> name;
    }
};

fn format_hdlr(auto hdlr) {
    if (std::core::has_member(hdlr, "name")) {
        return std::format("hdlr (type={}, name={})", hdlr.handler_type, hdlr.name.name);
    }
    return std::format("hdlr (type={})", hdlr.handler_type);
};



struct MediaHeaderAtom : FullAtom {

    u32 creation_time;
    u32 modification_time;
    u32 timescale;
    u32 duration;
    u16 language;
    u16 quality;

} [[format("format_mdhd")]];

fn format_mdhd(auto mdhd) {
    return std::format("mdhd {{ duration = {} }}",
        format_duration(mdhd.duration, mdhd.timescale));
};

fn format_duration(auto duration, auto timescale) {
    float duration_f = duration;
    float timescale_f = timescale;
    float total_seconds = duration_f / timescale_f;
    u128 minutes = std::math::floor(total_seconds / 60);
    float seconds = total_seconds - (minutes * 60);
    if (minutes > 0) {
        return std::format("{:02d}:{:06.3f}", minutes, seconds);
    } else {
        return std::format("{:.3f}s", seconds);
    }
};

fn format_mvhd(auto mvhd) {
    return "mvhd { duration = " + format_duration(mvhd.duration, mvhd.timescale) + " }";
};

struct MovieChildAtomSelector {
    Atom atom_header [[hidden]];

    $ = addressof(this);

    match(atom_header.box_type) {
        ("mvhd"): MovieHeaderAtom mvhd;
        ("trak"): TrackAtom trak;
        ("udta"): UserDataAtom udta;
        (_):  UnknownAtomSelector unknown [[name(atom_header.box_type)]];
    }
} [[inline]];

struct MovieAtom: Atom {
    MovieChildAtomSelector children[while(contents_size(addressof(this), box_size) > MIN_BOX_SIZE)];
};


// mdat
struct MediaDataAtom : Atom {
    u8 data[contents_size(addressof(this), box_size)] [[sealed]];
};

fn string_trim_end(ref str string, u128 amount) {
    u128 length = std::string::length(string);
    return std::string::substr(string, 0, length - amount);
};

fn format_box_array(auto atom_array) {
    str result = "[ ";
    u64 i;
    for (i = 0, i < std::core::member_count(atom_array), i += 1) {
        result = result + atom_array[i].atom_header.box_type + ", ";
    }
    return string_trim_end(result, 2) + " ]";
};


struct RootAtom {
    Atom atom_header [[hidden]];
    $ = addressof(this);
    if ($ + contents_size(addressof(this), atom_header.box_size) <= std::mem::size()) {

        match(atom_header.box_type) {
            ("moov"): MovieAtom moov;
            ("mdat"): MediaDataAtom mdat;
            (_): UnknownAtomSelector unknown [[name(atom_header.box_type)]];
        }
    } else {
        $ = std::mem::size();
    }

} [[inline]];

RootAtom atoms[while(std::mem::size() - $ > MIN_BOX_SIZE)] @ 0x00 [[format("format_box_array")]];

struct HotSpotEntry {
    u16 hotSpotId;
    padding[2];
    char type[4];
    u32 typeData;
    fixed_s32 viewHPan;
    fixed_s32 viewVPan;
    fixed_s32 viewZoom;
    u16 rect1;
    u16 rect2;
    u16 rect3;
    u16 rect4;
    s32 mouseOVerCursorID;
    s32 mouseDownCursorID;
    s32 mouseUpCursorID;
    padding[sizeof(s32)];
    s32 nameStrOffset;
    s32 commentStrOffset;
};

struct pHot: Atom {
    padding[2];
    u16 numHotSpots;
    HotSpotEntry entries[numHotSpots];
};

struct pHdr: Atom {
    u32 nodeID;
    fixed_s32 defHPan;
    fixed_s32 defVPan;
    fixed_s32 defZoom;
    fixed_s32 minHPan;
    fixed_s32 minVPan;
    fixed_s32 minZoom;
    fixed_s32 maxHPan;
    fixed_s32 maxVPan;
    fixed_s32 maxZoom;
    padding[sizeof(u32)];
    padding[sizeof(u32)];
    u32 nameStrOffset;
    u32 commentStrOffset;
};

struct PanoLink {
    u16 linkID;
    padding[sizeof(u16)];
    padding[sizeof(u32)];
    padding[sizeof(u32)];
    u32 toNodeID;
    padding[sizeof(u32) * 3];
    fixed_s32 toHPan;
    fixed_s32 toVPan;
    fixed_s32 toZoom;
    padding[sizeof(u32)];
    padding[sizeof(u32)];
    u32 nameStrOffset;
    u32 commentStrOffset;
};

struct LinkTableAtom : Atom {
    padding[sizeof(u16)];
    u16 numLinks;
    PanoLink entries[numLinks];
};

struct NavgObject {
    u16 objID;
    padding[sizeof(u16)]; // reserved1
    padding[sizeof(u32)]; // reserved2
    fixed_s32 navgHPan;
    fixed_s32 navgVPan;
    fixed_s32 navgZoom;
    u16 rect1;
    u16 rect2;
    u16 rect3;
    u16 rect4;
    padding[sizeof(u32)]; // reserved3
    u32 nameStrOffset;
    u32 commentStrOffset;
};

struct NavgTableAtom : Atom {
    padding[sizeof(u16)];
    u16 numObjects;
    NavgObject entries[numObjects];
};

struct PascalString {
    std::string::SizedString<u8> name;
};

struct StringTableAtom : Atom {
    PascalString children[while (contents_size(addressof(this), box_size) > MIN_BOX_SIZE)];
};

struct PanoramaMDAT {
    Atom atom_header [[hidden]];
    $ = addressof(this);
    if ($ + contents_size(addressof(this), atom_header.box_size) <= std::mem::size()) {

        match(atom_header.box_type) {
            ("pHot"): pHot phot;
            ("pHdr"): pHdr phdr;
            ("pLnk"): LinkTableAtom plnk;
            ("strT"): StringTableAtom strt;
            ("pNav"): NavgTableAtom pnav;
            (_): UnknownAtomSelector unknown [[name(atom_header.box_type)]];
        }
    } else {
        $ = std::mem::size();
    }

} [[inline]];
