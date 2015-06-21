module Main where

import           Data.Functor     ((<$>))
import           LogFind          (getAllFilesUnder, multipleStringsToPredicate)
import           System.Directory (getCurrentDirectory)
import           System.IO.Error  (catchIOError)

main :: IO ()
main = do
  confContent       <- readFile ".logfind" `catchIOError` const (return "")
  let filePredicate  = multipleStringsToPredicate . lines $ confContent
  
  filter filePredicate <$> (getCurrentDirectory >>= getAllFilesUnder) >>=  mapM_ putStrLn
