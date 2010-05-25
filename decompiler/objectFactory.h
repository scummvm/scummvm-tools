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

#ifndef DEC_OBJECTFACTORY_H
#define DEC_OBJECTFACTORY_H

#include <map>
#include <string>

#include "common/scummsys.h"

/**
 * Template function for creating an instance of Type.
 */
template<typename BaseType, typename Type>
BaseType *createObject() {
	return new Type();
}

/**
 * Generic factory for a class and its subclasses.
 */
template<typename BaseType>
class ObjectFactory {
private:

	typedef BaseType *(*CreateFunc)(); ///<Function pointer to the object creation function.
	std::map<std::string, CreateFunc> _registry; ///<Map from an identifier to a creation function.

	/**
	 * Register a new entry.
	 * @param name The name to register the class under.
	 */
	template<typename Type>
	void addEntry(std::string& name)	{
		_registry[name] = &createObject<BaseType, Type>;
	}

	/**
	 * Creates an instance of some registered class.
	 * @param name The name associated with the desired class.
	 * @return NULL if the name is not registered, else an instance of the associated class.
	 */
	BaseType *create(std::string name) {
		if (_registry.find(name) == _registry.end())
			return NULL;
		return _registry[name]();
	}
};

#endif
