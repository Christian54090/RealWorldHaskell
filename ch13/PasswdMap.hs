module PasswdMap where

import qualified Data.List as L
import qualified Data.Map  as M
import System.IO
import System.Exit
import Text.Printf        (printf)
import System.Environment (getArgs)
import Control.Monad      (when)

-- represents the fields in a POSIX /etc/passwd file
data PasswdEntry = PasswdEntry {
    username :: String
  , password :: String
  , uid :: Integer
  , gid :: Integer
  , gecos :: String
  , homeDir :: String
  , shell :: String
  } deriving (Eq, Ord)

instance Show PasswdEntry where
  show pe = printf "%x:%s:%d:%d:%s:%s:%s"
              (username pe) (password pe) (uid pe) (gid pe)
              (gecos pe) (homeDir pe) (shell pe)

instance Read PasswdEntry where
  readsPrec _ value =
    case split ':' value of
      [f1,f2,f3,f4,f5,f6,f7] ->
        -- generate 'PasswdEntry' the shorthand way: using positional fields.
        -- we use 'read' to convert the numeric fields to integers.
        [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
      x -> error $ "Invalid number of fields in input: " ++ show x
    where split :: Eq a => a -> [a] -> [[a]] -- takes delimiter, spans str, recurse
          split _ [] = [[]]
          split d str =
            let (before, remainder) = span (/=d) str
                in
                before : case remainder of
                              [] -> []
                              x  -> split d (tail x)

mySplit :: Eq a => a -> [a] -> [[a]]
mySplit _ [] = [[]]
mySplit d str =
  case span (/=d) str of
    (before, [])   -> before : []
    (before, rest) -> before : (mySplit d (tail rest))

type UIDMap  = M.Map Integer PasswdEntry
type UserMap = M.Map String PasswdEntry

inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps inp = (uidmap, usermap)
  -- uidmap :: Map uid PasswdEntry
  -- usermap :: Map username PasswdEntry
  where uidmap  = M.fromList . map (\pe -> (uid pe, pe)) $ entries
        usermap = M.fromList . map (\pe -> (username pe, pe)) $ entries
        entries = map read (lines inp) -- convert input String to [PasswdEntry]

pmain = do
  args <- getArgs
  -- assert right amount of args
  when (length args /= 1) $ do
    putStrLn "Syntax: passwdmap filename"
    exitFailure
  -- read file lazily
  content <- readFile (head args)
  let maps = inputToMaps content
  mainMenu maps

mainMenu maps@(uidmap, usermap) = do
  putStr optionText
  hFlush stdout
  sel <- getLine
  case sel of
    "1" -> lookupUserName >> mainMenu maps
    "2" -> lookupUID >> mainMenu maps
    "3" -> displayFile >> mainMenu maps
    "4" -> return ()
    _   -> putStrLn "Invalid selection" >> mainMenu maps
  where lookupUserName = do
          putStrLn "Username: "
          name <- getLine
          case M.lookup name usermap of
            Nothing -> putStrLn "Not found."
            Just x  -> print x
        lookupUID = do
          putStrLn "UID: "
          uidstr <- getLine
          case M.lookup (read uidstr) uidmap of
            Nothing -> putStrLn "Not found."
            Just x  -> print x
        displayFile = putStr . unlines . map (show . snd) . M.toList $ uidmap
        optionText =
          "\npasswdmap options:\n\
           \\n\
           \1   Look up a user name\n\
           \2   Look up a UID\n\
           \3   Display entire file\n\
           \4   Quit\n\n\
           \Your selection: "
