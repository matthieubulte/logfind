module Main where

import           Data.Functor       ((<$>))
import           LogFind            (getAllFilesUnder, matchesAll,
                                     multipleStringsToPredicate)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)
import           System.IO.Error    (catchIOError)

main :: IO ()
main = do

  arguments         <- getArgs
  let filePredicate = matchesAll arguments

  confContent       <- readFile ".logfind" `catchIOError` const (return "")
  let pathPredicate  = multipleStringsToPredicate . lines $ confContent

  allPaths          <- getCurrentDirectory >>= getAllFilesUnder
  let matchingPaths  = filter pathPredicate allPaths

  allFiles          <- zip matchingPaths <$> mapM readFile matchingPaths
  let matchingFiles  = filter (filePredicate . snd) allFiles

  let finalPaths     = fmap fst matchingFiles

  mapM_ putStrLn finalPaths
