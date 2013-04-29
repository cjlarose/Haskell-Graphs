import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Data.Map (fromList)
import Data.List
import Data.Char
import Data.Maybe (fromMaybe)
import Control.Monad
import Text.Printf
import qualified Draw
import qualified GraphGen

--main = getArgs >>= parse
main = do
        (flags, files) <- getArgs >>= parse
        putStrLn $ "Flags: " ++ show flags
        putStrLn $ "Files: " ++ show files
        let files = if null files then ["-"] else files
        if help flags
            then do hPutStrLn stderr (usageInfo header options)
                    exitWith ExitSuccess
            else return (nub [flags], files)

env = Data.Map.fromList `fmap` getEnvironment

header = concat ["\nExample usage:\n",
                 "\t\t./main -g tree -n 10 -i 50 -d 0\n\n",
                 "Available Options:"]

graphTypes = ["list", "cycle", "star", "complete", "tree"]

data Options = Options
    { tweak       :: Int ,
      delay       :: Int ,
      iterations  :: Int ,
      width       :: Int ,
      height      :: Int ,
      nodes       :: Int ,
      file        :: Maybe FilePath ,
      socFile     :: Maybe FilePath ,
      graph       :: String ,
      help        :: Bool
    } deriving (Show, Eq)

defaultOptions = Options
    { tweak        = 0 ,
      delay        = 100 ,
      iterations   = 20 ,
      width        = 500 ,
      height       = 500 ,
      nodes        = 5 ,
      file         = Nothing ,
      socFile      = Nothing ,
      graph        = "List" ,
      help         = False
    }

options :: [OptDescr (Options -> Options)]
options =
        [Option ['C'] [] (ReqArg (\x o -> o {tweak = read x}) "<TWEAK>")
            "Tweak factor. [0..1]",
         Option ['d'] [] (ReqArg (\x o -> o {delay = read x}) "<DELAY>")
            "Delay in milliseconds between graphs. Default is 100.",
         Option ['i'] [] (ReqArg (\x o -> o {iterations = read x}) "<#ITERATIONS>")
            "Number of iterations. Default is 20.",
         Option ['W'] [] (ReqArg (\x o -> o {width = read x}) "<WINDOW WIDTH>")
            "Window width. Defualt is 500.",
         Option ['H'] [] (ReqArg (\x o -> o {height = read x}) "<WINDOW HEIGHT>")
            "Window height. Default is 500.",
         Option ['n'] [] (ReqArg (\x o -> o {nodes = read x}) "<#NODES>")
            "Number of nodes in the graph. Default is 5.",
         Option ['f'] [] (ReqArg (\x o -> o {file = Just x}) "<FILENAME>")
            "Specify a file to load a graph from.",
         Option ['s'] [] (ReqArg (\x o -> o {socFile = Just x}) "<FILENAME>")
            "Specify a file to load a social graph from.",
         Option ['g'] [] (ReqArg (\x o -> o {graph =  x}) "<TYPE>")
            "Choose a graph type. Default is linked list.",
         Option ['h'] [] (NoArg (\o -> o {help = True}))
            "Print help and exit."
        ]


-- -- -- -- -- -- -- -- -- --
--   Command Line Parsing  --
-- -- -- -- -- -- -- -- -- --

parse argv = case getOpt Permute options argv of
    (args,fs,[]) -> return (foldl (flip id) defaultOptions args, fs)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

--   do
--       let files = if null fs then ["-"] else fs
--       if Help `elem` args then
--           do hPutStrLn stderr (usageInfo header flags)
--              >> exitWith ExitSuccess
--       else
--           Draw.createWindow graph 640 480 20 0
--
--   (_,_,errs) -> do
--Define our graph for later usage
(g,_ ,_) = GraphGen.star 7

--parse ["-g"] = do params <- sequence []

-- -- -- -- -- -- -- -- -- --
--     Exit Definitions    --
-- -- -- -- -- -- -- -- -- --
exit = exitWith ExitSuccess
die  = exitWith (ExitFailure 1)
