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

#ifndef DEC_GRAPH_H
#define DEC_GRAPH_H

#include "instruction.h"
#include "refcounted.h"

#include <ostream>
#include <utility>
#include <vector>

#include <boost/format.hpp>
#include <boost/version.hpp>

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>

#include <boost/intrusive_ptr.hpp>

/**
 * Enumeration representing the different kinds of groups.
 */
enum GroupType {
	kNormalGroupType,      ///< Normal group.
	kWhileCondGroupType,   ///< Group is the condition check for a while-loop.
	kDoWhileCondGroupType, ///< Group is the condition check for a do-while-loop.
	kIfCondGroupType,      ///< Group is the condition check for an if.
	kBreakGroupType,       ///< Group is a break.
	kContinueGroupType     ///< Group is a continue.
};

struct Group;

/**
 * Pointer to a Group.
 */
typedef boost::intrusive_ptr<Group> GroupPtr;

/**
 * Type representing properties containing a pointer to a Group.
 */
typedef boost::property<boost::vertex_name_t, GroupPtr> GroupProperty;

/**
 * Type representing properties containing an index, followed by a GroupProperty.
 */
typedef boost::property<boost::vertex_index_t, int, GroupProperty> GraphProperty;

/**
 * Structure representing whether or not an edge is a jump.
 */
struct IsJump {
	bool _isJump; ///< Whether or not the edge is a jump.

	/**
	 * Parameterless constructor for Group. Required for use with STL and Boost, should not be called manually.
	 */
	IsJump() { _isJump = false; };

	/**
	 * Constructor for IsJump.
	 *
	 * @param isJump Whether or not the edge is a jump.
	 */
	IsJump(bool isJump) : _isJump(isJump) {};

	/**
	 * Output edge information to an std::ostream as a graphviz edge property.
	 *
	 * @param output The std::ostream to output to.
	 * @param isJump The IsJump to output.
	 * @return The std::ostream used for output.
	 */
	friend std::ostream &operator<<(std::ostream &output, IsJump isJump) {
		if (isJump._isJump)
			output << "empty";
		else
			output << "normal";
		return output;
	}
};

namespace boost {

/**
 * Property writer for the isJump property.
 */
template <class Name>
class arrowheadWriter {
public:

	/**
	 * Constructor for arrowheadWriter.
	 *
	 * @param _name The name of the attribute to use.
	 */
	arrowheadWriter(Name _name) : name(_name) {}

	/**
	 * Outputs the arrowhead edge property.
	 *
	 * @param out The std::ostream to output to.
	 * @param v   The vertex or edge to output the attribute for.
	 */
	template <class VertexOrEdge>
	void operator()(std::ostream& out, const VertexOrEdge& v) const {
		out << "[arrowhead=\"" << get(name, v) << "\"]";
	}
private:
	Name name; ///< The name of the attribute to use.
};

/**
 * Creates an arrowhead property writer.
 *
 * @param _name The name of the attribute to use.
 */
template <class Name>
inline arrowheadWriter<Name>
makeArrowheadWriter(Name n) {
	return arrowheadWriter<Name>(n);
}

} // End of namespace boost

typedef boost::property<boost::edge_attribute_t, IsJump> EdgeProperty;

/**
 * Type used for the code flow graph.
 */
typedef boost::adjacency_list<boost::setS, boost::listS, boost::bidirectionalS, GraphProperty, EdgeProperty> Graph;

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
 * Structure representing a line of code.
 */
struct CodeLine {
	std::string _line;    ///< The line of code.
	bool _unindentBefore; ///< Whether or not to add an indentation level before outputting the line.
	bool _indentAfter;    ///< Whether or not to remove an indentation level after outputting the line.

	/**
	 * Constructor for CodeLine.
	 *
	 * @param line The line of code.
	 * @param unindentBefore Whether or not to remove an indentation level before the line. Defaults to false.
	 * @param indentAfter Whether or not to add an indentation level after the line. Defaults to false.
	 */
	CodeLine(const std::string& line, bool unindentBefore, bool indentAfter) {
		_line = line;
		_unindentBefore = unindentBefore;
		_indentAfter = indentAfter;
	}
};

/**
 * Weak references to groups ending else blocks.
 */
typedef std::vector<Group *> ElseEnds;

/**
 * Iterator type for ElseEnds
 */
typedef ElseEnds::iterator ElseEndIterator;

/**
 * Structure representing a group of instructions.
 */
struct Group : public RefCounted {
public:
	GraphVertex _vertex;         ///< Vertex the group belongs to.
	ConstInstIterator _start;    ///< First instruction in the group.
	ConstInstIterator _end;      ///< Last instruction in the group.
	int _stackLevel;             ///< Level of the stack upon entry.
	GroupType _type;             ///< Type of the group.
	bool _startElse;             ///< Group is start of an else block.
	ElseEnds _endElse;           ///< Group is end of an else block.
	Group *_prev;                ///< Pointer to the previous group, when ordered by address. Used for short-circuit analysis.
	Group *_next;                ///< Pointer to the next group, when ordered by address.
	std::vector<CodeLine> _code; ///< Container for decompiled lines of code.
	bool _coalescedElse;         ///< True if an else starting has been coalesced with another block (e.g. "else if"). If true, an else starting here should not be closed explicitly, but left to the other block.

	/**
	 * Parameterless constructor for Group. Required for use with STL and Boost, should not be called manually.
	 */
	Group() : _stackLevel(-1), _type(kNormalGroupType) { }

	/**
	 * Constructor for Group.
	 *
	 * @param v     The vertex the group belongs to.
	 * @param start First instruction in the group.
	 * @param end   Last instruction in the group.
	 * @param prev  Pointer to the previous group, when ordered by address.
	 */
	Group(GraphVertex v, ConstInstIterator start, ConstInstIterator end, GroupPtr prev) {
		_vertex = v;
		_start = start;
		_end = end;
		_stackLevel = -1;
		_type = kNormalGroupType;
		_prev = prev.get();
		_startElse = false;
		if (_prev != NULL)
			_prev->_next = this;
		_next = NULL;
		_coalescedElse = false;
	}

	/**
	 * Output a group to an std::ostream as a graphviz label.
	 *
	 * @param output The std::ostream to output to.
	 * @param group  The Group to output.
	 * @return The std::ostream used for output.
	 */
	friend std::ostream &operator<<(std::ostream &output, GroupPtr group) {
		output << "{Block type: ";
		switch(group->_type) {
		case kNormalGroupType:
			output << "Normal";
			break;
		case kWhileCondGroupType:
			output << "While condition";
			break;
		case kDoWhileCondGroupType:
			output << "Do-while condition";
			break;
		case kIfCondGroupType:
			output << "If condition";
			break;
		case kBreakGroupType:
			output << "Break";
			break;
		case kContinueGroupType:
			output << "Continue";
			break;
		}
		output << "\\n";
		output << "Expected stack level: " << group->_stackLevel << "\\n";
		if (group->_startElse)
			output << "Start of else\\n";
		for (ElseEndIterator it = group->_endElse.begin(); it != group->_endElse.end(); ++it) {
			output << boost::format("End of else at %08x\\n") % (*(*it)->_start)->_address;
		}
		output << "|";
		ConstInstIterator inst = group->_start;
		do {
			std::stringstream stream;
			stream << *inst;
			if (BOOST_VERSION >= 104500)
				output << stream.str();
			else {
				std::string s = stream.str();
				for (std::string::iterator it = s.begin(); it != s.end(); ++it)
					if (*it == '"')
						output << "\\\"";
					else if (*it == '|')
						output << "\\|";
					else if (*it == '{')
						output << "\\{";
					else if (*it == '}')
						output << "\\}";
					else
						output << *it;
			}
			output << "\\n";
		} while (inst++ != group->_end);
		output << "}";
		return output;
	}
};

class Engine;

/**
 * Type used to set properties for dot output.
 */
struct GraphProperties {
private:
	Engine *_engine; ///< Pointer to the engine containing function information for the script.
	const Graph &_g; ///< Const reference to the graph for the script.

public:
	/**
	 * Constructor for GraphProperties.
	 *
	 * @param engine Pointer to the engine containing function information for the script.
	 * @param g Const reference to the graph for the script.
	 */
	GraphProperties(Engine *engine, const Graph &g) : _engine(engine), _g(g) {
	}

	/**
	 * Called by write_graphviz from Boost.Graph to print properties of the graph.
	 *
	 * @param out The std::ostream write_graphviz is writing to.
	 */
	void operator()(std::ostream& out) const;
};

#endif
