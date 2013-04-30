HC = ghc
HCFLAGS = -O2
EXECUTABLES = main main_prof

main: Main.hs Graph.hs GraphGen.hs GraphPhysics.hs GraphDraw.hs Point.hs
	$(HC) $(HCFLAGS) main.hs

main_prof: Main.hs Graph.hs GraphGen.hs GraphPhysics.hs GraphDraw.hs Point.hs
	$(HC) $(HCFLAGS) -prof -auto-all -o main_prof main.hs

.PHONY : all
all: $(EXECUTABLES)

.PHONY : clean
clean:
	/bin/rm -f *.hi *.o $(EXECUTABLES) *.prof
