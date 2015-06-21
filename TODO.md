### TODO

##### research
+ ~~find the best way to test if a file contains several words (AND feature)~~: for each word, create a regex to test against the file, each of the regex should find at least one match
+ ~~find the best way to test if a file contains some words (OR feature)~~: create one regex by joining each word with `|`
+ ~~find a unit test library~~: [hspec](https://hspec.github.io/) seems to be a good library, also found [a great article](https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/en/tutorial.md) about it. The article also presents [doctest](https://github.com/sol/doctest) which also seems to be a great tool.
+ ~~find a regular expression library~~: [regex-posix](https://hackage.haskell.org/package/regex-posix) seems to be the easiest way to go.
+ ~~find a library to decode command line arguments~~: [getArgs](https://hackage.haskell.org/package/base-4.8.0.0/docs/System-Environment.html#v:getArgs) from `base` should be enough as there is at most one flag to read.
+ ~~find out how to test IO code~~: no satifying result here, I'll just make sure no have a little impure code as possible, that's anyway a good practice.


##### implementation
+ ~~create cabal file~~
+ ~~create a function to find all files in a directory and its sub-directories~~
+ ~~create a function to filter files based on the list of regexes used to find files~~
+ create a function creating a predicate for files the AND feature
+ create a function creating a predicate over files for the OR feature
