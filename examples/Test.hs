module Test where

import System.Directory (canonicalizePath)

-- add :: Int -> Int -> Int
add x y = x + y

pwd :: IO FilePath
pwd = canonicalizePath "."
