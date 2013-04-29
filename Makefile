HC = ghc
HCFLAGS =
EXECUTABLES = testdraw simple main lines

main: main.hs graph.hs graphgen.hs gdraw.hs draw.hs point.hs
	$(HC) $(HCFLAGS) main.hs

testdraw: testdraw.hs
	$(HC) $(HCFLAGS) testdraw.hs

simple: SimpleGraphics.hs
	$(HC) $(HCFLAGS) -o simple SimpleGraphics.hs

.PHONY : all
all: $(EXECUTABLES)

.PHONY : clean
clean:
	/bin/rm -f *.hi *.o $(EXECUTABLES)
