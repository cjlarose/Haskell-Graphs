Assignment 4 - Haskelly Paskell
===============================

Authors
-------
* Chris LaRose
* Roey D. Chasman

Instructions
------------
```bash
cabal update
cabal install OpenGL
cabal install GLFW
cabal install split
cabal install suspend
cabal install timers
make
./main -h
```

Options
-------
    Available Options:
      -h                                     Print help and exit.
                        --sierpinski         A special function
      -C <TWEAK>                             Sets the tweak factor. Between [0..1]. (1.0 Default)
      -d <DELAY>                             Delay in milliseconds between graphs. (100 Default)
      -i <#ITERATIONS>                       Number of iterations. (20 Default)
      -W <WIDTH>                             Window width. (500 Default)
      -H <HEIGHT>                            Window height. (500 Default).
      -n <#NODES>                            Number of nodes in the graph. (5 Default).
      -f <FILENAME>                          Specify a file to load a graph from.
      -s <FILENAME>                          Specify a file to load a social graph from.
      -c <COLOR>        --nodeColor=<COLOR>  Pick a color for the nodes to be: (Blue Default)
                                             Red
                                             Blue
                                             Green
                                             Yellow
                                             White
                                             Magenta
                                             Cyan
                                             Black
                        --edgeColor=<COLOR>  Pick a color for the edges to be: (White Default)
                                             Red
                                             Blue
                                             Green
                                             Yellow
                                             White
                                             Magenta
                                             Cyan
                                             Black
      -g <TYPE>                              Choose a graph type: (list Default)
                                             list
                                             cycle
                                             star
                                             complete
                                             tree

License
-------
FILES NOT FOR INDIVIDUAL RESALE
