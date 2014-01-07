/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
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

#ifndef REFCOUNTED_H
#define REFCOUNTED_H

class RefCounted;

inline void intrusive_ptr_add_ref(RefCounted *p);
inline void intrusive_ptr_release(RefCounted *p);

/**
 * Provides a base implementation of reference counting for use with boost::intrusive_ptr.
 */
class RefCounted {
private:
	long _refCount; ///< Reference count used for boost::intrusive_ptr.
	friend void ::intrusive_ptr_add_ref(RefCounted *p); ///< Allow access by reference counting methods.
	friend void ::intrusive_ptr_release(RefCounted *p); ///< Allow access by reference counting methods.

protected:
	RefCounted() : _refCount(0) { }
	virtual ~RefCounted() { }
};

/**
 * Add a reference to a pointer.
 */
inline void intrusive_ptr_add_ref(RefCounted *p) {
	++(p->_refCount);
}

/**
 * Remove a reference from a pointer.
 */
inline void intrusive_ptr_release(RefCounted *p) {
	if (--(p->_refCount) == 0)
		delete p;
}

#endif
