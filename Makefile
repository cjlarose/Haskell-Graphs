GC = ghc
GCFLAGS =

testdraw: testdraw.hs
	$(GC) $(GCFLAGS) testdraw.hs

.PHONY : clean
clean:
	/bin/rm -f *.hi *.o testdraw
