/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include "engine.h"
#include "disassembler.h"
#include <vector>

namespace Groovie {

GroovieEngine::GroovieEngine() {
	setOutputStackEffect(false);
}

void GroovieEngine::getVariants(std::vector<std::string> &variants) const {
	variants.push_back("t7g");
	variants.push_back("v2");
}

Disassembler *GroovieEngine::getDisassembler(InstVec &insts) {
	return new GroovieDisassembler(insts, getOpcodes());
}

CodeGenerator *GroovieEngine::getCodeGenerator(std::ostream &output) {
	return NULL;
}

const std::vector<GroovieOpcode> &GroovieEngine::getOpcodes() const {
	if (!_variant.compare("t7g"))
		return opcodesT7G;
	else if (!_variant.compare("v2"))
		return opcodesV2;
	else
		//return NULL;
		return opcodesT7G;
}

const std::vector<GroovieOpcode> GroovieEngine::opcodesT7G = {
	{0x00, kKernelCallInst, "Nop",                   ""},
	{0x01, kKernelCallInst, "Nop",                   ""},
	{0x02, kKernelCallInst, "PlaySong",              "2"},
	{0x03, kKernelCallInst, "BitFlag 9 ON",          ""},
	{0x04, kKernelCallInst, "Palette Fade Out",      ""},
	{0x05, kKernelCallInst, "BitFlag 8 ON",          ""},
	{0x06, kKernelCallInst, "BitFlag 6 ON",          ""},
	{0x07, kKernelCallInst, "BitFlag 7 ON",          ""},
	{0x08, kKernelCallInst, "SetBackgroundSong",     "2"},
	{0x09, kKernelCallInst, "VideoFromRef",          "2"},
	{0x0A, kKernelCallInst, "BitFlag 5 ON",          ""},
	{0x0B, kKernelCallInst, "Input Loop Start",      ""},
	{0x0C, kCondJumpInst,   "Keyboard Action",       "1@"},
	{0x0D, kCondJumpInst,   "Hotspot Rect",          "2222@1"},
	{0x0E, kCondJumpInst,   "Hotspot Left",          "@"},
	{0x0F, kCondJumpInst,   "Hotspot Right",         "@"},
	{0x10, kCondJumpInst,   "Hotspot Center",        "@"},
	{0x11, kCondJumpInst,   "Hotspot Center",        "@"},
	{0x12, kCondJumpInst,   "Hotspot Current",       "@"},
	{0x13, kJumpInst,       "Input Loop End",        ""},
	{0x14, kKernelCallInst, "Random",                "31"},
	{0x15, kJumpInst,       "Jmp",                   "@"},
	{0x16, kKernelCallInst, "LoadString",            "3A"},
	{0x17, kReturnInst,     "Return",                "1"},
	{0x18, kCallInst,       "Call",                  "@"},
	{0x19, kKernelCallInst, "Sleep",                 "2"},
	{0x1A, kCondJumpInst,   "JmpStrCmp-NE",          "3A@"},
	{0x1B, kKernelCallInst, "XOR Obfuscate",         "3A"},
	{0x1C, kKernelCallInst, "VDX Transition",        "2"},
	{0x1D, kKernelCallInst, "Swap",                  "22"},
	{0x1E, kKernelCallInst, "Nop8",                  "1"},
	{0x1F, kUnaryOpPreInst, "Inc",                   "3"},
	{0x20, kUnaryOpPreInst, "Dec",                   "3"},
	{0x21, kCondJumpInst,   "JmpStrCmpVar-NE",       "2A@"},
	{0x22, kKernelCallInst, "Copy BG to FG",         ""},
	{0x23, kCondJumpInst,   "JmpStrCmp-EQ",          "3A@"},
	{0x24, kKernelCallInst, "Mov",                   "32"},
	{0x25, kBinaryOpInst,   "Add",                   "32"},
	{0x26, kKernelCallInst, "VideoFromString1",      "V"},
	{0x27, kKernelCallInst, "VideoFromString2",      "V"},
	{0x28, kKernelCallInst, "Nop16",                 "2"},
	{0x29, kKernelCallInst, "Stop MIDI",             ""},
	{0x2A, kKernelCallInst, "End Script",            ""},
	{0x2B, kKernelCallInst, "Nop",                   ""},
	{0x2C, kCondJumpInst,   "Set Hotspot Top",       "@1"},
	{0x2D, kCondJumpInst,   "Set Hotspot Bottom",    "@1"},
	{0x2E, kKernelCallInst, "Load Game",             "3"},
	{0x2F, kKernelCallInst, "Save Game",             "3"},
	{0x30, kCondJumpInst,   "Hotspot Bottom 4",      "@"},
	{0x31, kKernelCallInst, "MIDI Volume",           "22"},
	{0x32, kCondJumpInst,   "JNE",                   "32@"},
	{0x33, kKernelCallInst, "Load String Var",       "3A"},
	{0x34, kCondJumpInst,   "JmpCharGreat",          "3A@"},
	{0x35, kKernelCallInst, "BitFlag 7 OFF",         ""},
	{0x36, kCondJumpInst,   "JmpCharLess",           "3A@"},
	{0x37, kKernelCallInst, "Copy Rect to BG",       "2222"},
	{0x38, kKernelCallInst, "Restore Stack Pointer", ""},
	{0x39, kKernelCallInst, "Obscure Swap",          "CCCC"},
	{0x3A, kKernelCallInst, "Print String",          "A"},
	{0x3B, kCondJumpInst,   "Hotspot Slot",          "12222@1"},
	{0x3C, kKernelCallInst, "Check Valid Saves",     ""},
	{0x3D, kKernelCallInst, "Reset Variables",       ""},
	{0x3E, kBinaryOpInst,   "Mod",                   "31"},
	{0x3F, kKernelCallInst, "Load Script",           "S"},
	{0x40, kKernelCallInst, "Set Video Origin",      "22"},
	{0x41, kBinaryOpInst,   "Sub",                   "32"},
	{0x42, kKernelCallInst, "Cell Move",             "1"},
	{0x43, kKernelCallInst, "Return Script",         "1"},
	{0x44, kCondJumpInst,   "Set Hotspot Right",     "@"},
	{0x45, kCondJumpInst,   "Set Hotspot Left",      "@"},
	{0x46, kKernelCallInst, "Nop",                   ""},
	{0x47, kKernelCallInst, "Nop",                   ""},
	{0x48, kKernelCallInst, "Nop8",                  "1"},
	{0x49, kKernelCallInst, "Nop",                   ""},
	{0x4A, kKernelCallInst, "Nop16",                 "2"},
	{0x4B, kKernelCallInst, "Nop8",                  "1"},
	{0x4C, kKernelCallInst, "Get CD",                ""},
	{0x4D, kKernelCallInst, "Play CD",               "1"},
	{0x4E, kKernelCallInst, "Music Delay",           "2"},
	{0x4F, kKernelCallInst, "Nop16",                 "2"},
	{0x50, kKernelCallInst, "Nop16",                 "2"},
	{0x51, kKernelCallInst, "Nop16",                 "2"},
	{0x52, kKernelCallInst, "UNKNOWN52",             "1"},
	{0x53, kCondJumpInst,   "Hotspot OutRect",       "2222@"},
	{0x54, kKernelCallInst, "Nop",                   ""},
	{0x55, kKernelCallInst, "Nop16",                 "2"},
	{0x56, kKernelCallInst, "Stub56",                "411"},
	{0x57, kKernelCallInst, "UNKNOWN57",             "4"},
	{0x58, kKernelCallInst, "UNKNOWN58",             ""},
	{0x59, kKernelCallInst, "UNKNOWN59",             "31"},
};

const std::vector<GroovieOpcode> GroovieEngine::opcodesV2 = {
	{0x00, kKernelCallInst, "Invalid",               ""},
	{0x01, kKernelCallInst, "Nop",                   ""},
	{0x02, kKernelCallInst, "PlaySong",              "4"},
	{0x03, kKernelCallInst, "Nop",                   ""},
	{0x04, kKernelCallInst, "Nop",                   ""},
	{0x05, kKernelCallInst, "Nop",                   ""},
	{0x06, kKernelCallInst, "Nop",                   ""},
	{0x07, kKernelCallInst, "Nop",                   ""},
	{0x08, kKernelCallInst, "SetBackgroundSong",     "4"},
	{0x09, kKernelCallInst, "VideoFromRef",          "4"},
	{0x0A, kKernelCallInst, "BitFlag 0 ON",          ""},
	{0x0B, kKernelCallInst, "Input Loop Start",      ""},
	{0x0C, kCondJumpInst,   "Keyboard Action",       "1@"},
	{0x0D, kCondJumpInst,   "Hotspot Rect",          "2222@1"},
	{0x0E, kCondJumpInst,   "Hotspot Left",          "@"},
	{0x0F, kCondJumpInst,   "Hotspot Right",         "@"},
	{0x10, kCondJumpInst,   "Hotspot Center",        "@"},
	{0x11, kCondJumpInst,   "Hotspot Center",        "@"},
	{0x12, kCondJumpInst,   "Hotspot Current",       "@"},
	{0x13, kJumpInst,       "Input Loop End",        ""},
	{0x14, kKernelCallInst, "Random",                "31"},
	{0x15, kJumpInst,       "Jmp",                   "@"},
	{0x16, kKernelCallInst, "LoadString",            "3A"},
	{0x17, kReturnInst,     "Return",                "1"},
	{0x18, kCallInst,       "Call",                  "@"},
	{0x19, kKernelCallInst, "Sleep",                 "2"},
	{0x1A, kCondJumpInst,   "JmpStrCmp-NE",          "3A@"},
	{0x1B, kKernelCallInst, "XOR Obfuscate",         "3A"},
	{0x1C, kKernelCallInst, "VDX Transition",        "4"},
	{0x1D, kKernelCallInst, "Swap",                  "22"},
	{0x1E, kKernelCallInst, "Invalid",               "1"},
	{0x1F, kUnaryOpPreInst, "Inc",                   "3"},
	{0x20, kUnaryOpPreInst, "Dec",                   "3"},
	{0x21, kCondJumpInst,   "JmpStrCmpVar-NE",       "2A@"},
	{0x22, kKernelCallInst, "Copy BG to FG",         ""},
	{0x23, kCondJumpInst,   "JmpStrCmp-EQ",          "3A@"},
	{0x24, kKernelCallInst, "Mov",                   "32"},
	{0x25, kBinaryOpInst,   "Add",                   "32"},
	{0x26, kKernelCallInst, "VideoFromString1",      "V"},
	{0x27, kKernelCallInst, "VideoFromString2",      "V"},
	{0x28, kKernelCallInst, "Invalid",               "2"},
	{0x29, kKernelCallInst, "Nop",                   ""},
	{0x2A, kKernelCallInst, "End Script",            ""},
	{0x2B, kKernelCallInst, "Invalid",               ""},
	{0x2C, kCondJumpInst,   "Set Hotspot Top",       "@1"},
	{0x2D, kCondJumpInst,   "Set Hotspot Bottom",    "@1"},
	{0x2E, kKernelCallInst, "Load Game",             "3"},
	{0x2F, kKernelCallInst, "Save Game",             "3"},
	{0x30, kCondJumpInst,   "Hotspot Bottom 4",      "@"},
	{0x31, kKernelCallInst, "MIDI Control",          "22"},
	{0x32, kCondJumpInst,   "JNE",                   "32@"},
	{0x33, kKernelCallInst, "Load String Var",       "3A"},
	{0x34, kCondJumpInst,   "JmpCharGreat",          "3A@"},
	{0x35, kKernelCallInst, "BitFlag 7 OFF",         ""},
	{0x36, kCondJumpInst,   "JmpCharLess",           "3A@"},
	{0x37, kKernelCallInst, "Copy Rect to BG",       "2222"},
	{0x38, kKernelCallInst, "Restore Stack Pointer", ""},
	{0x39, kKernelCallInst, "Obscure Swap",          "CCCC"},
	{0x3A, kKernelCallInst, "Print String",          "22111V"},
	{0x3B, kCondJumpInst,   "Hotspot Slot",          "12222@1"},
	{0x3C, kKernelCallInst, "Check Valid Saves",     ""},
	{0x3D, kKernelCallInst, "Reset Variables",       ""},
	{0x3E, kBinaryOpInst,   "Mod",                   "31"},
	{0x3F, kKernelCallInst, "Load Script",           "S"},
	{0x40, kKernelCallInst, "Set Video Origin",      "22"},
	{0x41, kBinaryOpInst,   "Sub",                   "32"},
	{0x42, kKernelCallInst, "Cell Move",             "1"},
	{0x43, kKernelCallInst, "Return Script",         "1"},
	{0x44, kCondJumpInst,   "Set Hotspot Right",     "@"},
	{0x45, kCondJumpInst,   "Set Hotspot Left",      "@"},
	{0x46, kKernelCallInst, "Invalid",               ""},
	{0x47, kKernelCallInst, "Invalid",               ""},
	{0x48, kKernelCallInst, "Invalid",               "1"},
	{0x49, kKernelCallInst, "Invalid",               ""},
	{0x4A, kKernelCallInst, "Nop16",                 "2"},
	{0x4B, kKernelCallInst, "Invalid",               "1"},
	{0x4C, kKernelCallInst, "Invalid",               ""},
	{0x4D, kKernelCallInst, "Invalid",               "1"},
	{0x4E, kKernelCallInst, "Invalid",               "2"},
	{0x4F, kKernelCallInst, "Save Screen",           "2"},
	{0x50, kKernelCallInst, "Restore Screen",        "2"},
	{0x51, kCondJumpInst,   "Set Video Skip",        "@"},
	{0x52, kKernelCallInst, "Copy FG to BG",         "1"},
	{0x53, kCondJumpInst,   "Hotspot OutRect",       "2222@"},
	{0x54, kKernelCallInst, "Invalid",               ""},
	{0x55, kKernelCallInst, "Set Script End",        "2"},// should this be an @?
	{0x56, kKernelCallInst, "Play Sound",            "411"},
	{0x57, kKernelCallInst, "Invalid",               "4"},
	{0x58, kKernelCallInst, "Wipe Mask From String", "V"},
	{0x59, kKernelCallInst, "Check Sounds Overlays", "31"},
	{0x5A, kKernelCallInst, "Preview Loadgame",      "1"},
};

} // End of namespace Groovie
