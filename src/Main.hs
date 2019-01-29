module Main where 

import System.Environment
import Game

main :: IO ()
main = do
  args <- getArgs
  parseArgs args

parseArgs [] = startGame
parseArgs y@(x:_) = do
  case x of
    "--help" -> usage
    "--version" -> version
    _ -> startGame

usage :: IO()
usage = putStrLn usageStr

usageStr :: String
usageStr = "Правила игры.В этой игре играют 2 игрока,\n" ++
           "каждый бросает кубик за ход, пока кто нибудь не\n" ++
           "наберет пороговое значение(вводите в начале игры)\n"
            
           
version :: IO()
version = putStrLn versionStr

versionStr :: String
versionStr = "Dice.\n" ++
             "Version 4.2.2.1\n" ++
             "(c) Kolmogorov Denis, 2018"
