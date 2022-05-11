module Main (main) where

import Iris (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
