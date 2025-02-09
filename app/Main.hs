module Main (main) where

import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Debug.Trace
import System.Environment (getArgs)
import System.IO

-- | The main entry point of the program.
-- It processes command line arguments and either displays help or processes the given file.
main :: IO ()
main = do
  args <- getArgs
  if "--help" `elem` args || "-h" `elem` args
    then printHelp
    else do
      fileName <- case args of
        (arg : _) -> return arg
        [] -> do
          putStr "Enter file name: "
          hFlush stdout
          getLine
      content <- trace fileName readFile fileName
      putStrLn $ "Creating files based on " ++ fileName ++ ": " ++ content
      applyRequest $ parseRequest fileName content
      putStrLn "done"

-- | Prints the help message with usage information.
printHelp :: IO ()
printHelp = do
  putStrLn "Usage: book-notes-generator [OPTIONS] [FILE]"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  -h, --help    Show this help message and exit"
  putStrLn ""
  putStrLn "Arguments:"
  putStrLn "  FILE          The path to the file to process. If not provided, you will be prompted to enter the file name."

-- | Data type representing a request to generate files.
data Request = Request
  { -- | The number of files to generate.
    amount :: Int,
    -- | Function to generate the content of each file.
    fileContent :: Int -> String,
    -- | Function to generate the name of each file.
    fileName :: Int -> String
  }

-- | Applies the request by generating the specified number of files with the given content and names.
applyRequest :: Request -> IO ()
applyRequest r =
  sequence_ $
    map
      ( \i ->
          let name = fileName r i
              content = fileContent r i
           in writeFile name content
      )
      [1 .. (amount r - 1)]

type FileName = String

type FileContent = String

-- | Parses the request from the given file name and content.
-- It determines the number of files to generate, and the functions to generate file names and content.
parseRequest :: FileName -> FileContent -> Request
parseRequest name content =
  let amount = read $ last $ words $ head $ splitOn "." name
      fileName i =
        let withoutExtention = head $ splitOn "." name
            extention = '.' : (last $ splitOn "." name)
            withoutNumber = unwords $ init $ words withoutExtention
         in withoutNumber ++ " " ++ show i ++ extention
      fileContent i =
        let line = head $ filter (\s -> listToMaybe s == Just '#') (lines content)
            beforeHeadingContent = head $ splitOn line content
            afterHeadingContent = unwords $ tail $ splitOn line content
            newLine = (unwords $ init $ words line) ++ " " ++ show i
         in beforeHeadingContent ++ newLine ++ afterHeadingContent
   in Request
        { amount = amount,
          fileName = fileName,
          fileContent = fileContent
        }
