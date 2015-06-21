module LogFind where

main :: IO ()
main = putStrLn "Ola"

-- |
-- This returns True
--
-- >>> truth ()
-- True
truth :: () -> Bool
truth = const True
