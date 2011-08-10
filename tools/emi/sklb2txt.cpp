#include <fstream>
#include <string>
#include <iostream>

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
	int strLength = 0;
	int numBones = 0;
	
	file.read((char*)&numBones,4);
	
	
	char boneString[32];
	char parentString[32];
	
	float x = 0, y = 0, z = 0, angle = 0;
	// Bones are listed in the same order as in the meshb.
	for(int i=0;i<numBones;i++){
		file.read((char*)&boneString,32);
		file.read((char*)&parentString,32);
		
		std::cout << "# BoneName " << boneString << "\twith parent: " << parentString << "\t"; 
		//file.seekg(28,ios::cur);
		std::cout << " position: ";
		file.read((char*)&x,4);
		file.read((char*)&y,4);
		file.read((char*)&z,4);
		std::cout << x << " " << y << " " << z << " ";
		std::cout << " rotation: ";
		file.read((char*)&x,4);
		file.read((char*)&y,4);
		file.read((char*)&z,4);
		std::cout << x << " " << y << " " << z << " angle: ";
		file.read((char*)&angle,4);
		std::cout << angle << std::endl;

	}
	/*
	The last 28 bytes are, by looking at guy.sklb:
	 INT(0) <-- This has another value for 1 entry in el1.sklb.
	 INT(0) <-- except for the second entry, pelvis.
	 
	 */
}