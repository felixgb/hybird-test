module Main where

import Server

main :: IO ()
main = readPointsCsv "points.csv" >>= serve
