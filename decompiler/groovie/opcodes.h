/* ScummVM Tools
 * Copyright (C) 2010 The ScummVM project
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

namespace Groovie {

struct GroovieOpcode {
	uint32 opcode;
	int type;
	const char *name;
	const char *params;
		// 1 = 8 bits
		// 2 = 16 bits
		// 3 = 8 or 16 bits
		// 4 = 32 bits
		// @ = Address
		// A = Array
		// S = Script name
		// V = Video name
		// C = Indexed value
};

} // End of namespace Groovie
