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

#ifndef SCI_UNPACK_H_
#define SCI_UNPACK_H_

#include <sciresource.h>

#define SORT_METHOD_ALPHA 0
#define SORT_METHOD_GROUP 1

#define DEFAULT_SORTING SORT_METHOD_ALPHA

extern int vocab_sort; /* Sorting strategy for vocab */
extern resource_mgr_t *resmgr;

int
vocab_print(void);
/* Prints vocab data
** Parameters: (void)
** Returns   : (int) 0 on success, 1 on failure
** Controlled by vocab_sort
*/

int
script_dump(void);
/* Prints all object information
** Parameters: (void)
** Returns   : (int) 0 on success, 1 on failure
*/

int
vocab_dump(void);
/* Prints full vocabulary information
** Parameters: (void)
** Returns   : (int) 0 on success, 1 on failure
*/

int
print_classes(void);
/* Prints full class information
** Parameters: (void)
** Returns   : (int) 0 on success, 1 otherwise
*/

#endif
