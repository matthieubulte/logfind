### TODO

##### research
+ ~~find the best way to test if a file contains several words (AND feature)~~: for each word, create a regex to test against the file, each of the regex should find at least one match
+ ~~find the best way to test if a file contains some words (OR feature)~~: create one regex by joining each word with `|`
+ ~~find a unit test library~~: [hspec](https://hspec.github.io/) seems to be a good library, also found [a great article](https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/en/tutorial.md) about it. The article also presents [doctest](https://github.com/sol/doctest) which also seems to be a great tool.
+ find a regular expression library
+ find a library to decode command line arguments
+ find out how to test IO code


##### implementation
+ create cabal file
+ create a function to find all files in a directory and its sub-directories
+ create a function to filter files based on the list of regexes used to find files
+ create a function creating a predicate for files the AND feature
+ create a function creating a predicate over files for the OR feature
