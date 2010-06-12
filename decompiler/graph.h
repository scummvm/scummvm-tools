/* ScummVM Tools
 * Copyright (C) 2010 The ScummVM project
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *
 */

#ifndef DEC_GRAPH_H
#define DEC_GRAPH_H

#include "instruction.h"

#include <ostream>

#include <boost/format.hpp>

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>

/**
 * Structure representing a group of instructions.
 */
struct Group {
	InstIterator _start; ///< First instruction in the group.
	InstIterator _end;   ///< Last instruction in the group.

	Group() {}

	/**
	 * Constructor for Group.
	 * @param start First instruction in the group.
	 * @param end Last instruction in the group.
	 */
	Group(InstIterator start, InstIterator end) {
		_start = start;
		_end = end;
	}

	/**
	 * Output a group to an std::ostream as a graphviz label.
	 * @param output The std::ostream to output to.
	 * @param group The Group to output.
	 * @return The std::ostream used for output.
	 */
	friend std::ostream& operator<< (std::ostream &output, Group &group) {
		InstIterator inst = group._start;
		do {
			output << boost::format("%08x: %s") % inst->_address % inst->_name;
			std::vector<Parameter>::iterator param;
			for (param = inst->_params.begin(); param != inst->_params.end(); ++param) {
				if (param != inst->_params.begin())
					output << ",";
				output << " " << param->_value;
			}
			output << "\\n";			
		} while (inst++ != group._end);
		return output;
	}
};

typedef boost::property<boost::vertex_name_t, Group> GroupProperty; ///< Type representing properties containing a Group

typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::bidirectionalS, GroupProperty> Graph; ///< Type used for the code flow graph
typedef Graph::vertex_descriptor GraphVertex; ///< Type representing a vertex in the graph
typedef Graph::edge_descriptor GraphEdge; ///< Type representing an edge in the graph

#endif
