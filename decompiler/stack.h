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

#ifndef DEC_STACK_H
#define DEC_STACK_H

#include <deque>

/**
 * Stack class based on a deque.
 */
template<typename T>
class Stack {
private:
	std::deque<T> _stack; ///< Container used for the stack.
public:

	/**
	 * Returns whether or not the stack is empty.
	 *
	 * @return true if the stack is empty, false if it is not.
	 */
	bool empty() const { return _stack.empty(); }

	/**
	 * Push an item onto the stack.
	 *
	 * @param item The item to push.
	 */
	void push(const T &item) { _stack.push_back(item); }

	/**
	 * Pop an item from the stack and return it.
	 *
	 * @return The value popped from the stack.
	 */
	T pop() {
		T retval = _stack.back();
		_stack.pop_back();
		return retval;
	}

	/**
	 * Return the topmost item on the stack without removing it.
	 *
	 * @return The topmost item on the stack.
	 */
	T &peek() { return _stack.back(); }

	/**
	 * Return the topmost item on the stack without removing it.
	 *
	 * @return The topmost item on the stack.
	 */
	const T &peek() const { return _stack.back(); }
};

#endif
