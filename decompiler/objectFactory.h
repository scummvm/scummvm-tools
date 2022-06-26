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
template<typename KeyType, typename BaseType>
class ObjectFactory {
private:

	/**
	 * Function pointer to the object creation function.
	 */
	typedef BaseType *(*CreateFunc)();

	/**
	 * Type used to store registered entries.
	 */
	typedef std::map<KeyType, CreateFunc> RegistryMap;

	RegistryMap _registry; ///< Map from an identifier to a creation function.

public:
	/**
	 * Register a new entry.
	 *
	 * @param key The key to register the class under.
	 */
	template<typename Type>
	void addEntry(const KeyType &key) {
		_registry[key] = &createObject<BaseType, Type>;
	}

	/**
	 * Creates an instance of some registered class.
	 *
	 * @param key The key associated with the desired class.
	 * @return NULL if the name is not registered, else an instance of the associated class.
	 */
	BaseType *create(const KeyType &key) const {
		typename RegistryMap::const_iterator entry = _registry.find(key);
		if (entry == _registry.end())
			return NULL;
		return (entry->second)();
	}
};

#endif
