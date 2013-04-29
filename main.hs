import System.Environment
import System.Console.GetOpt
import System.Exit
import qualified Draw
import qualified GraphGen
import Data.Map (fromList)

main = getArgs >>= parse

env = Data.Map.fromList `fmap` getEnvironment

data Flag
        = Help           -- -h
        | Tweak          -- -C n
        | Delay          -- -d n
        | Iterations     -- -i n
        | Width          -- -W n
        | Height         -- -H n
        | Nodes          -- -n n
        | List           -- -g list
        | Cycle          -- -g cycle
        | Star           -- -g star
        | K              -- -g complete
        | Tree           -- -g tree
        | File           -- -f file
        | SocFile        -- -s file
        deriving (Eq, Ord, Enum, Show, Bounded)

--Define our graph for later usage
(graph,_ ,_) = GraphGen.star 7

-- -- -- -- -- -- -- -- -- --
--   Command Line Parsing  --
-- -- -- -- -- -- -- -- -- --
--parse ("-f":num:f1:f2:[]) = visualize' (read num :: Int) f1 f2 >> exit
--parse (num:f1:f2:[]) = visualize (read num :: Int) f1 f2 >> exit
parse ["-h"] = usage >> exit
parse _ = Draw.createWindow graph 640 480 20 0
--parse ["-g"] = do params <- sequence []

-- -- -- -- -- -- -- -- -- --
--    Usage Definitions    --
-- -- -- -- -- -- -- -- -- --
usage        = mapM_ putStrLn usageStrings
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

-- -- -- -- -- -- -- -- -- --
--     Exit Definitions    --
-- -- -- -- -- -- -- -- -- --
exit = exitWith ExitSuccess
die  = exitWith (ExitFailure 1)
