/* Residual - A 3D game interpreter
*
* Residual is the legal property of its developers, whose names
* are too numerous to list here. Please refer to the AUTHORS
* file distributed with this source distribution.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
*
* $URL:
* $Id:
*
*/

#ifndef MODEL_H
#define MODEL_H

#include <string>
#include <stdint.h>

struct Colormap {
	unsigned char r, g, b, a;
};

class Mesh;
class Lab;

class Material {
	uint32_t *_texIDs;
	uint32_t _numTextures;
	Lab *_lab;
	void loadTGATexture(std::string filename);
	void loadSURTexture(std::string filename);
public:
	Material() { _lab = 0; }
	void setLab(Lab *lab) { _lab = lab; }
	void loadTexture(std::string filename);
	void bindTexture(int index = 0);
};

class MeshFace {
	Vector3<int> *_indexes;
	uint32_t _faceLength;
	uint32_t _numFaces;
	uint32_t _hasTexture;
	uint32_t _texID;
	uint32_t _flags;
	Mesh *_parent;
public:
	MeshFace() : _numFaces(0), _hasTexture(0), _texID(0), _flags(0) { }
	void loadFace(std::istream *file);
	void setParent(Mesh *m) { _parent = m; }
	void render();
};

class Mesh {
	int _numVertices;
	Vector3d *_vertices;
	Vector3d *_normals;
	Colormap *_colorMap;
	Vector2d *_texVerts;

	// Stuff I dont know how to use:
	Vector4d *_sphereData;
	Vector3d *_boxData;
	Vector3d *_boxData2;
	int _numTexSets;
	int _setType;

	uint32_t _numFaces;
	MeshFace *_faces;
	uint32_t _numTextures;
	std::string *_texNames;
	Material *_mats;
	Lab *_lab;
public:
	Mesh() { _lab = 0; }
	void setLab(Lab *lab) { _lab = lab; }
	void setTex(int index) { _mats[index].bindTexture(); }
	void loadMesh(std::string fileName);
	void prepare();
	void render();
};

#endif
