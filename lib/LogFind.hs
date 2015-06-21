module LogFind ( getAllFilesUnder
               , multipleStringsToPredicate
               , matchesAll
               , matchesSome
               ) where

import           Data.Functor
import           Data.List        (partition)
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           Text.Regex.Posix ((=~))

-- |
-- Join two file paths the Unix way using "/".
--
-- >>> "a" </> "b"
-- "a/b"
(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b


-- |
-- Tell whether a file is a reference to another directory (., ..)
--
-- >>> isCurrentOrParentFolder "."
-- True
--
-- >>> isCurrentOrParentFolder ".."
-- True
--
-- >>> isCurrentOrParentFolder "banana"
-- False
isCurrentOrParentFolder :: FilePath -> Bool
isCurrentOrParentFolder f = f == "." || f == ".."


-- |
-- Returns a list of the file paths of the files under a directory, the result
-- is split in a tuple of file paths of directories and file paths of files.
getDirectorySeperatedContent :: FilePath -> IO ([FilePath], [FilePath])
getDirectorySeperatedContent directory = do
 allFilePaths            <- filter (not . isCurrentOrParentFolder) <$> getDirectoryContents directory
 filePathIsDirectory     <- mapM doesDirectoryExist allFilePaths

 let absoluteFilePaths    = (directory </>) <$> allFilePaths
 let (directories, files) = partition snd (zip absoluteFilePaths filePathIsDirectory)

 return (fst <$> directories, fst <$> files)


-- |
-- Finds recursively all files under a given directory.
getAllFilesUnder :: FilePath -> IO [FilePath]
getAllFilesUnder directory = do
 (directories, files)  <- getDirectorySeperatedContent directory
 subChildren           <- sequence $ getAllFilesUnder <$> directories

 return $ files ++ concat subChildren


-- |
-- Converts a string to a predicate testing if a filename is to include in
-- the result set
--
-- >>> (stringToPredicate "-xyz") "xyz"
-- False
--
-- >>> (stringToPredicate "+xyz") "xyz"
-- True
--
-- >>> (stringToPredicate "xyz") "xyz"
-- True
--
-- >>> (stringToPredicate "^xy+z$") "xyyyyyyyyyyz"
-- True
stringToPredicate :: String -> String -> Bool
stringToPredicate ('-':regex) = not . (=~ regex)
stringToPredicate ('+':regex) = (=~ regex)
stringToPredicate regex       = (=~ regex)


-- |
-- Converts multiple string to a predicate testing if a filename is to include in
-- the result set. The filename satisties the predicate if each of the predicate
-- created for each element of the list was satisfied.
--
-- Putting a "-" in front of a line will negate the predicate.
-- Putting a "+" or nothing in front of a line will have no effect.
--
-- >>> (multipleStringsToPredicate ["-xyz", "+z"]) "xyz"
-- False
--
-- >>> (multipleStringsToPredicate ["-xyz", "+z"]) "yz"
-- True
--
-- >>> (multipleStringsToPredicate []) "yz"
-- True
multipleStringsToPredicate :: [String] -> String -> Bool
multipleStringsToPredicate xs = and . sequence (stringToPredicate <$> xs)

-- |
-- Converts multiple string to a predicate testing if a filename is to include in
-- the result set. The filename satisties the predicate if each of the predicate
-- created for each element of the list was satisfied.
--
-- >>> (matchesAll ["y", "z"]) "xyz"
-- True
--
-- >>> (matchesAll ["^y", "z"]) "xyz"
-- False
--
-- >>> (matchesAll []) "xyz"
-- True
matchesAll :: [String] -> String -> Bool
matchesAll xs = and . sequence (flip (=~) <$> xs)


-- |
-- Converts multiple string to a predicate testing if a filename is to include in
-- the result set. The filename satisties the predicate if any of the predicate
-- created for each element of the list was satisfied.
--
-- >>> (matchesSome ["y", "a"]) "xyz"
-- True
--
-- >>> (matchesSome ["^y", "z"]) "xyz"
-- True
--
-- >>> (matchesSome ["^y", "b"]) "xyz"
-- False
--
-- >>> (matchesSome []) "xyz"
-- True
matchesSome :: [String] -> String -> Bool
matchesSome [] = const True
matchesSome xs = or . sequence (flip (=~) <$> xs)
