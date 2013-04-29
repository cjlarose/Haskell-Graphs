import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Data.Map (fromList)
import Data.List
import Data.Char
import Control.Monad
import Text.Printf
import qualified Draw
import qualified GraphGen

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

flags = [Option ['h'] []           (NoArg Help)
            "Print help and exit.",
         Option ['C'] []           (NoArg Tweak)
            "Tweak factor. [0..1]",
         Option ['d'] []           (NoArg Delay)
            "Delay in milliseconds between graphs. Default is 100.",
         Option ['i'] []           (NoArg Iterations)
            "Number of iterations. Default is 20.",
         Option ['W'] []           (NoArg Width)
            "Window width. Defualt is 500.",
         Option ['H'] []           (NoArg Height)
            "Window height. Default is 500.",
         Option ['n'] []           (NoArg Nodes)
            "Number of nodes in the graph. Default is 5.",
         Option ['g'] []           (NoArg List)
            "Generate a linked list graph. Default.",
         Option ['g'] []           (NoArg Cycle)
            "Generate a simple cycle graph.",
         Option ['g'] []           (NoArg Star)
            "Generate a star graph.",
         Option ['g'] []           (NoArg K)
            "Generate a complete graph.",
         Option ['g'] []           (NoArg Tree)
            "Generate a binary tree.",
         Option ['f'] []           (NoArg File)
            "Load a graph from a file.",
         Option ['s'] []           (NoArg SocFile)
            "Load a social graph from a file."]

parse argv = case getOpt Permute flags argv of
    (args,fs,[]) -> do
        let files = if null fs then ["-"] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitWith ExitSuccess
            else return (nub args, files)

    (_,_,errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)
    where header = "Example usage:\n\t\tmain -g tree -n 10 -i 50 -d 0\n"

--Define our graph for later usage
(graph,_ ,_) = GraphGen.star 7

-- -- -- -- -- -- -- -- -- --
--   Command Line Parsing  --
-- -- -- -- -- -- -- -- -- --
--parse ("-f":num:f1:f2:[]) = visualize' (read num :: Int) f1 f2 >> exit
--parse (num:f1:f2:[]) = visualize (read num :: Int) f1 f2 >> exit
--parse ["-h"] = usage >> exit
--parse _ = Draw.createWindow graph 640 480 20 0
--parse ["-g"] = do params <- sequence []

-- -- -- -- -- -- -- -- -- --
--     Exit Definitions    --
-- -- -- -- -- -- -- -- -- --
exit = exitWith ExitSuccess
die  = exitWith (ExitFailure 1)
