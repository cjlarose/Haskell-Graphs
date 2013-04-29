import System.Environment
import System.Exit
import qualified Draw
import qualified GraphGen

main = getArgs >>= parse

parse ["-h"] = usage >> exit
--parse ("-f":num:f1:f2:[]) = visualize' (read num :: Int) f1 f2 >> exit
--parse (num:f1:f2:[]) = visualize (read num :: Int) f1 f2 >> exit
--parse _ = usage >> exit
parse _ = Draw.initWindow graph 640 480 20 0
    where (graph, a , b) = GraphGen.list 15

usage   = mapM_ putStrLn usageStrings

usageStrings =
    ["-h           : Print help and exit.",
     "-C n         : Tweak factor. [0..1]",
     "-d n         : Delay in milliseconds between graph displays.",
     "               If n = 0 no intermediate graphs will be displayed.",
     "               Default is 100",
     "-i n         : Number of iterations. Default is 20.",
     "-W n         : Window width. Defualt is 500.",
     "-M n         : Window height. Default is 500.",
     "-n n         : Number of nodes in the graph. Defualt is 5.",
     "-g list      : Generate a linked list graph. Default.",
     "-g cycle     : Generate a simple cycle graph.",
     "-g star      : Generate a star graph.",
     "-g K         : Generate a complete graph.",
     "-g tree      : Generate a binary tree.",
     "-f <file>    : Load a graph from a file.",
     "-s <file>    : Load a social graph from a file.",
     "",
     "Example usage:",
     "      main -g tree -n 10 -i 50 -d 0"]
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
