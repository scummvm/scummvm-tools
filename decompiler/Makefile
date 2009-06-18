.PHONY: clean

DEPS = $(patsubst %.cc,%.o,$(*.cc))

#%.o: %.cc
#	g++ -Wall -g -c $< -o $@

decompiler: decompiler.o $(DEPS)
	g++ -Wall -g  -lboost_program_options $^ -o $@

decompiler.o: decompiler.cc misc.h instruction.h parser.h reader.h cfg.h graph.h
	g++ -Wall -g -c decompiler.cc -o decompiler.o

clean:
	rm -rf decompiler *.o
