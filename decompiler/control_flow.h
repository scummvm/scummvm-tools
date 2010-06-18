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

#ifndef DEC_CONTROL_FLOW_H
#define DEC_CONTROL_FLOW_H

#include "graph.h"
#include "engine.h"

/**
 * Class for doing code flow analysis.
 */
class ControlFlow {
private:
	Graph _g;                               ///< The control flow graph.
	Engine *_engine;                        ///< Pointer to the Engine used for the script.
	std::vector<Instruction> &_insts;       ///< The instructions being analyzed
	std::map<uint32, GraphVertex> _addrMap; ///< Map between addresses and vertices.

	/**
	 * Finds a graph vertex through an instruction.
	 *
	 * @param inst The instruction to find the vertex for.
	 */
	GraphVertex find(Instruction inst);

	/**
	 * Finds a graph vertex through an instruction iterator.
	 *
	 * @param it The iterator to find the vertex for.
	 */
	GraphVertex find(InstIterator it);

	/**
	 * Finds a graph vertex through an address.
	 *
	 * @param address The address to find the vertex for.
	 */
	GraphVertex find(uint32 address);

	/**
	 * Merges two graph vertices. g2 will be merged into g1.
	 *
	 * @param g1 The first vertex to merge.
	 * @param g2 The second vertex to merge.
	 */
	void merge(GraphVertex g1, GraphVertex g2);

	/**
	 * Sets the stack level for all instructions, using depth-first search.
	 *
	 * @param g     The GraphVertex to search from.
	 * @param level The stack level when g is reached.
	 */
	void setStackLevel(GraphVertex g, int level);

public:
	/**
	 * Gets the current control flow graph.
	 *
	 * @returns The current control flow graph.
	 */
	const Graph &getGraph() const { return _g; };

	/**
	 * Constructor for the control flow graph.
	 *
	 * @param insts  std::vector containing the instructions to analyze control flow for.
	 * @param engine Pointer to the Engine used for the script.
	 */
	ControlFlow(std::vector<Instruction> &insts, Engine *engine);

	/**
	 * Creates groups suitable for a stack-based machine.
	 */
	void createGroups();

	/**
	 * Performs control flow analysis.
	 *
	 * @returns The control flow graph after analysis.
	 */
	const Graph &analyze();
};

#endif
