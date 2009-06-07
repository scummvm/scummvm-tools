.PHONY: clean

DEPS = $(patsubst %.cc,%.o,$(*.cc))

#%.o: %.cc
#	g++ -Wall -g -c $< -o $@

decompiler: decompiler.o $(DEPS)
	g++ -Wall -g $^ -o $@

decompiler.o: decompiler.cc misc.h instruction.h parser.h
	g++ -Wall -g -c decompiler.cc -o decompiler.o

clean:
	rm -rf decompiler *.o
