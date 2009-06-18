#ifndef CONFIGURATION_H
#define CONFIGURATION_H

struct Configuration {
	Configuration();
	
	// While prepending with _ would be in line with the coding conventions
	// this class is just a glorified map with different types, so it seems
	// unnecessary.

	bool compressing;
	wxString selectedGame;
	wxString selectedTool;
};

inline Configuration::Configuration() {
	// Default values for all the settings

	compressing = false;

	selectedGame = wxT("");
	selectedTool = wxT("");
}

#endif
