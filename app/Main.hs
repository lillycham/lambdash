module Main (main) where

import Lambdash (version)

main :: IO ()
main = putStrLn ("lambdash " <> version)
