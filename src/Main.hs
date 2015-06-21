module Main where

import           LogFind          (getAllFilesUnder)
import           System.Directory (getCurrentDirectory)


main :: IO ()
main = getCurrentDirectory >>= getAllFilesUnder >>= mapM_ putStrLn
