import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Data.Map (fromList)
import Data.List
import Data.Graph
import Data.Char
import Data.Maybe (fromMaybe)
import Control.Monad
import Text.Printf
import SOE
import qualified GraphDraw
import qualified GraphGen
import qualified GraphPhysics (newGraphAnimation)
import Data.Int (Int64)

-- -- -- -- -- -- -- -- -- --
--   Command Line Parsing  --
-- -- -- -- -- -- -- -- -- --
main = do
    (flags, files) <- getArgs >>= parse
    -- DEBUG
    putStrLn $ "Flags: " ++ show flags
    putStrLn $ "Files: " ++ show files
    -- Help Flag
    if (help flags)
        then do hPutStrLn stderr usage
                exitWith ExitSuccess
        else return (nub [flags], files)
    -- Sierpinski Flag
    if (sierpinski flags)
        then do GraphDraw.sierpinskiTriangle (width flags) (height flags) (nodeColor flags)
                exitWith ExitSuccess
        else return ()
    -- Tweak Flag
    if (tweak flags) /= (tweak defaultOptions)
        then do putStrLn $ "TWEAKIGN " ++ show (tweak flags)
        else putStrLn $ "NOT TWEAKIGN"
    -- Delay Flag
    if (delay flags) /= (delay defaultOptions)
        then do putStrLn $ "DELAY " ++ show (delay flags)
        else putStrLn $ "NOT DELAY"
    -- Iterations Flag
    if (iterations flags) /= (iterations defaultOptions)
        then do putStrLn $ "IRETATING " ++ show (iterations flags)
        else putStrLn $ "NOT ITEARTING"
    -- Width Flag
    if (width flags) /= (width defaultOptions)
        then do putStrLn $ "WIDENING " ++ show (width flags)
        else putStrLn $ "NOT WIDENING"
    -- Height Flag
    if (height flags) /= (height defaultOptions)
        then do putStrLn $ "HEIGHING " ++ show (height flags)
        else putStrLn $ "NOT HEIGHING"
    -- Nodes Flag
    if (nodes flags) /= (nodes defaultOptions)
        then do putStrLn $ "NODES++ " ++ show (nodes flags)
        else putStrLn $ "NOT NODES"
    -- Graph Type Flag
    let gt = elemIndex (graph flags) graphTypes
    let (g,_,_) = defineGraph (graphTypes !! fromMaybe 0 gt) (nodes flags)
    -- File Flag
    if (file flags) /= (file defaultOptions)
        then do putStrLn $ "file " ++ show (file flags)
        else putStrLn $ "NOT file"
    -- Social File Flag
    if (socFile flags) /= (socFile defaultOptions)
        then do putStrLn $ "SOCfile " ++ show (socFile flags)
        else putStrLn $ "NOT SOCfile"

    ga <- GraphPhysics.newGraphAnimation g (width flags) (height flags) (iterations flags) (tweak flags)
    GraphDraw.createWindow ga (width flags) (height flags) (delay flags) (nodeColor flags) (edgeColor flags)

parse argv = case getOpt Permute options argv of
    (args,fs,[]) -> return (foldl (flip id) defaultOptions args, fs)
    (_,_,errs) -> blowUp errs

blowUp errs = ioError (userError (concat errs ++ usage))

defineGraph graph nodes
    | graph == "star" = GraphGen.star nodes
    | graph == "cycle" = GraphGen.cycle nodes
    | graph == "tree" = GraphGen.binaryTree nodes
    | graph == "complete" = GraphGen.complete nodes
    | otherwise = GraphGen.list nodes

-- -- -- -- -- -- -- -- -- --
--    Utility Functions    --
-- -- -- -- -- -- -- -- -- --

usage = usageInfo (concat header) options

header = ["\nExample usage:\n",
          "\t\t./main -g tree -n 10 -i 50 -d 0\n\n",
          "Available Options:"]

graphTypes = ["list", "cycle", "star", "complete", "tree"]
colors = [Red, Blue, Green, Yellow, White, Magenta, Cyan, Black]

-- -- -- -- -- -- -- -- -- --
--    Option Definitions   --
-- -- -- -- -- -- -- -- -- --

data Options = Options
    { tweak       :: Float,
      delay       :: Int64,
      iterations  :: Int,
      width       :: Int,
      height      :: Int,
      nodes       :: Int,
      file        :: Maybe FilePath,
      socFile     :: Maybe FilePath,
      graph       :: String,
      help        :: Bool,
      nodeColor   :: Color,
      edgeColor   :: Color,
      sierpinski  :: Bool
    } deriving (Show, Eq, Ord)

defaultOptions = Options
    { tweak        = 1.0,
      delay        = 100,
      iterations   = 20,
      width        = 500,
      height       = 500,
      nodes        = 5,
      file         = Nothing,
      socFile      = Nothing,
      graph        = head graphTypes,
      nodeColor    = Blue,
      edgeColor    = White,
      help         = False,
      sierpinski   = False
    }

options :: [OptDescr (Options -> Options)]
options = [Option ['h'] []
               (NoArg (\o -> o {help = True}))
               "Print help and exit.",
           Option [] ["sierpinski"]
               (NoArg (\o -> o {sierpinski = True}))
               "A special function",
           Option ['C'] []
               (ReqArg (\x o -> o {tweak = read x})
                                    "<TWEAK>")
               "Sets the tweak factor. Between [0..1]. (1.0 Default)",
           Option ['d'] []
               (ReqArg (\x o -> o {delay =
                                    if read x >= 0 && read x <= 10000000
                                        then read x
                                        else delay defaultOptions})
                                    "<DELAY>")
               "Delay in milliseconds between graphs. (100 Default)",
           Option ['i'] []
               (ReqArg (\x o -> o {iterations =
                                    if read x >= 0 && read x <= 10000000
                                        then read x
                                        else iterations defaultOptions})
                                    "<#ITERATIONS>")
               "Number of iterations. (20 Default)",
           Option ['W'] []
               (ReqArg (\x o -> o {width =
                                    if read x >= 100 && read x <= 3000
                                        then read x
                                        else width defaultOptions})
                                    "<WIDTH>")
               "Window width. (500 Default)",
           Option ['H'] []
               (ReqArg (\x o -> o {height =
                                    if read x >= 100 && read x <= 2500
                                        then read x
                                        else height defaultOptions})
                                    "<HEIGHT>")
               "Window height. (500 Default).",
           Option ['n'] []
               (ReqArg (\x o -> o {nodes =
                                    if read x >= 1 && read x <= 3000
                                        then read x
                                        else nodes defaultOptions})
                                    "<#NODES>")
               "Number of nodes in the graph. (5 Default).",
           Option ['f'] []
               (ReqArg (\x o -> o {file =
                                    if length x > 0
                                        then Just x
                                        else file defaultOptions})
                                    "<FILENAME>")
               "Specify a file to load a graph from.",
           Option ['s'] []
               (ReqArg (\x o -> o {socFile =
                                    if length x > 0
                                        then Just x
                                        else socFile defaultOptions})
                                    "<FILENAME>")
               "Specify a file to load a social graph from.",
           Option ['c'] ["nodeColor"]
               (ReqArg (\x o -> o {nodeColor =
                                    if x `elem` (map (show) colors)
                                        then read x
                                        else nodeColor defaultOptions})
                                    "<COLOR>")
               (unlines $
                    ["Pick a color for the nodes to be: (Blue Default)"] ++ (map (show) colors)),
           Option [] ["edgeColor"]
               (ReqArg (\x o -> o {edgeColor =
                                    if x `elem` (map (show) colors)
                                        then read x
                                        else edgeColor defaultOptions})
                                    "<COLOR>")
               (unlines $
                    ["Pick a color for the edges to be: (White Default)"] ++ (map (show) colors)),
           Option ['g'] []
               (ReqArg (\x o -> o {graph =
                                    if x `elem` graphTypes
                                        then x
                                        else graph defaultOptions})
                                    "<TYPE>")
               (unlines $
                    ["Choose a graph type: (list Default)"] ++ graphTypes)]

-- -- -- -- -- -- -- -- -- --
--     Exit Definitions    --
-- -- -- -- -- -- -- -- -- --
exit = exitWith ExitSuccess
die  = exitWith (ExitFailure 1)
