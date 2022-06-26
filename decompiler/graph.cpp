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
