#include <iostream>

#include <boost/program_options.hpp>

using namespace std;
using namespace boost::program_options;

int main(int argc, char** argv) {

	options_description visible("Options");
	visible.add_options()
		("help", "Produce this help message.")
		("engine", value<string>(), "Engine the script originates from.")
		("list", "List the supported engines.");

	options_description args("");
	args.add(visible).add_options()
		("inputfile", value<string>(), "Input file");

	positional_options_description filename;
	filename.add("inputfile", -1);	

	variables_map vm;
	try {
		store(command_line_parser(argc, argv).options(args).positional(filename).run(), vm);
		notify(vm);    
	} catch (std::exception& e) {
		std::cout << e.what();
	}
	
	if (vm.count("help") || !vm.count("inputfile")) {
		cout << "Usage: " << argv[0] << " [option...] file" << endl << endl;
		cout << args << "\n";
		return 1;
	}

	if (vm.count("list")) {
		cout << "TODO" << "\n";
		return 0;
	}

	if (!vm.count("engine")) {
		cout << "Engine must be specified." << "\n";
		return 2;
	} else {
		cout << "Engine selected: " <<	vm["engine"].as<string>() << "\n";
	}

	cout << "Input file is " << vm["inputfile"].as<string>() << "\n";

	return 0;
}
