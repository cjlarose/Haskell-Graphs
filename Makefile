HC = ghc
HCFLAGS =
EXECUTABLES = testdraw

testdraw: testdraw.hs
	$(HC) $(HCFLAGS) testdraw.hs

.PHONY : all
all: $(EXECUTABLES)

.PHONY : clean
clean:
	/bin/rm -f *.hi *.o $(EXECUTABLES)
