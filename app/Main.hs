module Main where

import Herms

import System.Environment (getArgs)

main :: IO ()
main = do
  testCmd <- getArgs
  case herms testCmd of
    Nothing -> help [""]
    Just io -> io
