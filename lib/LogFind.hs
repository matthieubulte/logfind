module LogFind ( getAllFilesUnder
               ) where

import           Data.Functor
import           Data.List        (partition)
import           System.Directory (doesDirectoryExist, getDirectoryContents)


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
-- >>> isFilePathRelative "."
-- True
--
-- >>> isFilePathRelative ".."
-- True
--
-- >>> isFilePathRelative "banana"
-- False
isFilePathRelative :: FilePath -> Bool
isFilePathRelative f = f == "." || f == ".."


-- |
-- Returns a list of the file paths of the files under a directory, the result
-- is split in a tuple of file paths of directories and file paths of files.
getDirectorySeperatedContent :: FilePath -> IO ([FilePath], [FilePath])
getDirectorySeperatedContent directory = do
 allFilePaths            <- filter (not . isFilePathRelative) <$> getDirectoryContents directory
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
