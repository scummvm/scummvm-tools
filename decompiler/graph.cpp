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

#include "graph.h"
#include "engine.h"

void GraphProperties::operator()(std::ostream& out) const {
	out << "node [shape=record]" << std::endl;
	for (FuncMap::iterator fn = _engine->_functions.begin(); fn != _engine->_functions.end(); ++fn) {
		int index = (boost::get(boost::vertex_index, _g, fn->second._v));
		out << "XXX" << index << " [shape=none, label=\"\", height=0]" << std::endl;
		out << "XXX" << index << " -> " << index << std::endl;
	}
}
