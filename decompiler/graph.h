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
#include <utility>

#include <boost/format.hpp>

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>

/**
 * Enumeration representing the different kinds of groups.
 */
enum GroupType {
	kNormal,      ///< Normal group.
	kWhileCond,   ///< Group is the condition check for a while-loop.
	kDoWhileCond, ///< Group is the condition check for a do-while-loop.
	kIfCond,      ///< Group is the condition check for an if.
	kBreak,       ///< Group is a break.
	kContinue     ///< Group is a continue.
};

/**
 * Structure representing a group of instructions.
 */
struct Group {
	InstIterator _start; ///< First instruction in the group.
	InstIterator _end;   ///< Last instruction in the group.
	int _stackLevel;     ///< Level of the stack upon entry.
	GroupType _type;     ///< Type of the group.
	bool _else;          ///< Group is start of an else.
	Group *_prev;        ///< Pointer to the previous group, when ordered by address. Used for short-circuit analysis.
	Group *_next;        ///< Pointer to the next group, when ordered by address.
	
	/**
	 * Parameterless constructor for Group. Required for use with STL and Boost, should not be called manually.
	 */
	Group() { 
		_stackLevel = -1;
		_type = kNormal;
	}

	/**
	 * Constructor for Group.
	 *
	 * @param start First instruction in the group.
	 * @param end   Last instruction in the group.
	 * @param prev  Pointer to the previous group, when ordered by address.
	 */
	Group(InstIterator start, InstIterator end, Group *prev) {
		_start = start;
		_end = end;
		_stackLevel = -1;
		_type = kNormal;
		_prev = prev;
		_else = false;
		if (_prev != NULL)
			_prev->_next = this;
		_next = NULL;
	}

	/**
	 * Output a group to an std::ostream as a graphviz label.
	 *
	 * @param output The std::ostream to output to.
	 * @param group  The Group to output.
	 * @return The std::ostream used for output.
	 */
	friend std::ostream &operator<<(std::ostream &output, Group *group) {
		output << "Block type: ";
		switch(group->_type) {
		case kNormal:
			output << "Normal";
			break;
		case kWhileCond:
			output << "While condition";
			break;
		case kDoWhileCond:
			output << "Do-while condition";
			break;
		case kIfCond:
			output << "If condition";
			break;
		case kBreak:
			output << "Break";
			break;
		case kContinue:
			output << "Continue";
			break;
		}
		output << "\\n";
		if (group->_else)
			output << "Start of else\\n";
		InstIterator inst = group->_start;
		do {
			output << boost::format("%08x: %s") % inst->_address % inst->_name;
			std::vector<Parameter>::iterator param;
			for (param = inst->_params.begin(); param != inst->_params.end(); ++param) {
				if (param != inst->_params.begin())
					output << ",";
				output << " ";
				if (param->_type != kString)
					output << param->_value;
				else {
					std::string s = param->getString();
					for (std::string::iterator it = s.begin(); it != s.end(); ++it)
						if (*it == '"')
							output << "\\\"";
						else
							output << *it;
				}
			}
			output << boost::format(" (%d)") % inst->_stackChange << "\\n";
		} while (inst++ != group->_end);
		return output;
	}
};

/**
 * Type representing properties containing a pointer to a Group.
 */
typedef boost::property<boost::vertex_name_t, Group *> GroupProperty;

/**
 * Type representing properties containing an index, followed by a GroupProperty.
 */
typedef boost::property<boost::vertex_index_t, int, GroupProperty> GraphProperty;

/**
 * Type used for the code flow graph.
 */
typedef boost::adjacency_list<boost::setS, boost::listS, boost::bidirectionalS, GraphProperty> Graph;

/**
 * Type representing a vertex in the graph.
 */
typedef Graph::vertex_descriptor GraphVertex;

/**
 * Type representing an iterator for vertices.
 */
typedef Graph::vertex_iterator VertexIterator;

/**
 * Type representing an edge in the graph.
 */
typedef Graph::edge_descriptor GraphEdge;

/**
 * Type representing an iterator for outgoing edges.
 */
typedef Graph::out_edge_iterator OutEdgeIterator;

/**
 * Type representing an iterator for ingoing edges.
 */
typedef Graph::in_edge_iterator InEdgeIterator;

/**
 * Type representing a range of vertices from boost::vertices.
 */
typedef std::pair<VertexIterator, VertexIterator> VertexRange;

/**
 * Type representing a range of edges from boost::out_edges.
 */
typedef std::pair<OutEdgeIterator, OutEdgeIterator> OutEdgeRange;

/**
 * Type representing a range of edges from boost::in_edges.
 */
typedef std::pair<InEdgeIterator, InEdgeIterator> InEdgeRange;

/**
 * Type used to set properties for dot output.
 */
struct GraphProperties {

	/**
	 * Called by write_graphviz from Boost.Graph to print properties of the graph.
	 *
	 * @param out The std::ostream write_graphviz is writing to.
	 */
	void operator()(std::ostream& out) const {
		out << "node [shape=box]" << std::endl;
		out << "XXX [shape=none, label=\"\", height=0]" << std::endl;
		out << "XXX -> 0" << std::endl;
	}
};

#endif
