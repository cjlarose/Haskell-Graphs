HC = ghc
HCFLAGS =
EXECUTABLES = testdraw simple main

testdraw: testdraw.hs
	$(HC) $(HCFLAGS) testdraw.hs

main: main.hs
	$(HC) $(HCFLAGS) main.hs

simple: SimpleGraphics.hs
	$(HC) $(HCFLAGS) -o simple SimpleGraphics.hs

.PHONY : all
all: $(EXECUTABLES)

.PHONY : clean
clean:
	/bin/rm -f *.hi *.o $(EXECUTABLES)
