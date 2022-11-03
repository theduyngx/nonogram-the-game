{-
   Module  : Main
   Author  : The Duy Nguyen <theduyn@student.unimelb.edu.au>
   Purpose : Nonogram - The Game, where user is able to enter commands and thereby
             finish the puzzle. The program also has built-in nonogram solver.

NONOGRAM - The Game follows the rule of any nonogram puzzle. Each row and column of
a rectangular board is marked with the respective lengths of the hint for each row/
column. Solving the puzzle means to complete the board given only these lengths. The
program will handle specific user's input to pass in these 'hints' lengths as puzzle
initialization, then the program makes sure the puzzle is solvable. After that, the
user can solve the puzzle by themselves with given commands in the program.
-}

module Main (main) where

import AlgoSolve
import UserSolve

-- * User input functions
-- | main entry to program
main :: IO ()
main = do
    printWelcome
    mainCmd

-- | main program with home screen command line processing
mainCmd :: IO ()
mainCmd = do
    command <- getLine
    case command of
         "quit"  -> putStrLn "quitting game..."
         "home"  -> main
         "help"  -> helpHome
                    >> mainCmd
         "start" -> putStrLn "Enter the hints for row and column:"
                    >> startGame
         _ ->
           putStrLn "Unknown command, please enter 'start', 'home' or 'quit'."
           >> mainCmd

-- | function initiated when user starts the game
startGame :: IO ()
startGame = do
    putStrLn "\t-- Row hint:"
    rowStr <- getLine
    let rowHint = read rowStr :: Hints
    putStrLn "\t-- Column hint:"
    colStr <- getLine
    let colHint = read colStr :: Hints
    let solution = nonogram rowHint colHint
    case solution of
         [] ->
            do
              putStrLn "\nThe hints can't produce a solution, please try another one."
              startGame
         _  ->
            do
              let r = length colHint
              let c = length rowHint
              let puzzle = replicate c (replicate r '_')
              printP puzzle rowHint colHint
              putStrLn "Enter your commands below:"
              state <- userSolve puzzle rowHint colHint
              case state of
                   "m"   -> main
                   "cmd" -> mainCmd
                   "sol" -> do printSolution solution
                               putStrLn "Here's the solution, start again?"
                               mainCmd
                   _     -> return ()

-- | welcome title screen of the game
printWelcome :: IO ()
printWelcome = putStr $ unlines [
    " _|      _|",
    " _|_|    _|    _|_|    _|_|_|      _|_|      _|_|_|  _|  _|_|    _|_|_|  _|_|_|  _|_|",
    " _|  _|  _|  _|    _|  _|    _|  _|    _|  _|    _|  _|_|      _|    _|  _|    _|    _|",
    " _|    _|_|  _|    _|  _|    _|  _|    _|  _|    _|  _|        _|    _|  _|    _|    _|",
    " _|      _|    _|_|    _|    _|    _|_|      _|_|_|  _|          _|_|_|  _|    _|    _|",
    "                                                 _|",
    "                                             _|_|",
    "Welcome to NONOGRAM - The Game!",
    "The rules follow nonogram puzzle rule.",
    "    -- For more instructions, enter 'help'.",
    "    -- To start the game, enter 'start'. To quit, enter 'quit'.",
    "    -- You can also return to homescreen by entering 'home' anytime. Enjoy!"]

-- | function initiated when user inputs the 'help' command in home screen
helpHome :: IO ()
helpHome = do
    putStrLn $ unlines [
       "Help:",
       "    start           starting the game",
       "    home            go to home screen",
       "    quit            quit game",
       "    help            display this help message"]
    help

-- | function executed when user asks to print out the solution
printSolution :: Puzzle -> IO ()
printSolution = mapM_ putStr