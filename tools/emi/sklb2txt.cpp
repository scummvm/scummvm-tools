#include <fstream>
#include <string>
#include <iostream>
#include "filetools.h"
// Based on Benjamin Haischs work on sklb-files.

using namespace std;

int main(int argc, char **argv) {
	if(argc<2){
		std::cout << "Error: filename not specified" << std::endl;
		return 0;
	}
	std::string filename=argv[1];
	
	std::fstream file(filename.c_str(), std::ios::in | std::ios::binary);
	
	if (!file.is_open()){
		std::cout << "Unable to open file " << filename << std::endl;
		return 0;
	}
	int numBones = readInt(file);
	
	char boneString[32];
	char parentString[32];

	float angle = 0;
	// Bones are listed in the same order as in the meshb.
	Vector3d *vec = 0;
	for(int i=0;i<numBones;i++) {
		file.read((char*)&boneString,32);
		file.read((char*)&parentString,32);
		
		std::cout << "# BoneName " << boneString << "\twith parent: " << parentString << "\t"; 
		//file.seekg(28,ios::cur);
		std::cout << " position: ";
		vec = readVector3d(file);
		std::cout << vec->toString();
		delete vec;
		std::cout << " rotation: ";
		vec = readVector3d(file);
		std::cout << vec->toString();
		delete vec;
		angle = readFloat(file);
		std::cout << angle << std::endl;

	}
	/*
	The last 28 bytes are, by looking at guy.sklb:
	 INT(0) <-- This has another value for 1 entry in el1.sklb.
	 INT(0) <-- except for the second entry, pelvis.
	 
	 */
}
