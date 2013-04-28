GC = ghc
GCFLAGS =
EXECUTABLES = testdraw

testdraw: testdraw.hs
	$(GC) $(GCFLAGS) testdraw.hs

.PHONY: all
all: $(EXECUTABLES)

.PHONY : clean
clean:
	/bin/rm -f *.hi *.o $(EXECUTABLES)
