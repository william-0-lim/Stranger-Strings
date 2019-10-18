-- Project CPSC312 Haskell
-- STRANGER STRINGS 
-- TaeGyun Lim - 25983157
-- Do Hoon Lee - 26404153

module Project where 
-- To run it, try:
-- ghci
-- :load Project

import System.IO
-- import Game
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.Char  (isSpace, toUpper)
import Data.Maybe (isNothing)
import RPS
import GUN
-- import GUN

-- type StrangerState = (Int)     -- life remaining, but you only have one life to begin with  
-- data Action = Action Int 
--               deriving (Eq)

-- data Result = Game StrangerState
--               deriving (Show)

-- type Player = StrangerState -> Action 

--------------------------------------------------------
-- simple_player :: Player 
-- simple_player (life) = Action 0

real_game :: StrangerState -> IO StrangerState
real_game (life) =
  do 
    putStrLn "Welcome to STRANGER STRING my - sorry what was your name again?"
    line <- getLine
    putStrLn ("Of course, how could I forget, Welcome to STRANGER STRING, "++line++".")
    getLine
    putStrLn ("You have a psycic power, " ++line++ ", use it to save the Hawkins!")
    line2 <- getLine
    first_stage (life)
    line3 <- getLine
    second_stage (life)


--------------------------------------------------------
first_stage :: StrangerState -> IO StrangerState
first_stage (life) =
  do
    putStrLn "You are encapuslated in a laboratory in Hawkins. Beat three guards and escape the laboratory!"
    getLine
    putStrLn "It is a simple game of rock, paper, scissors, but with your psychic power draw will be counted as a win."
    getLine
    putStrLn "You are against the first guard."
    runGame (Just Rock) Nothing (life)
    if (life) == 1
      then do 
        putStrLn "You passed the first one. You are against the second guard."
        runGame (Just Paper) Nothing (life)
        if (life) == 1 
          then do 
            putStrLn "You beat the second one! You are against the third guard."
            runGame (Just Rock) Nothing (life)
            if (life) == 1 
              then do 
                putStrLn "You have passed all the guards!!!"
                return (life)
              else do 
                putStrLn "DEAD by the third guard."
                update_state 0 (life)
          else do 
            putStrLn "DEAD by the second guard."
            update_state 0 (life)
      else do
        putStrLn "Dead by the first guard."
        update_state 0 (life)
 

second_stage :: StrangerState -> IO StrangerState
second_stage (life) =
  do
      putStrLn "You have successfully escaped the laboratory. You encounter Mike, Lucas, Dustin, who are looking for their friend Will."
      line <- getLine
      putStrLn "Will is kidnapped by a Demogorgon to somewhere called Upside Down. You have to open the portal to go to the Upside Down."
      line1 <- getLine
      putStrLn "Here you have to answer three questions correct in order to open the gate. Good luck."
      line2 <- getLine
      putStrLn "What is my middle name?"
      line1 <- getLine
      if (line1 `elem` ["handsome"])
        then do 
          putStrLn ""
          putStrLn "Well done, here is question 2: I'm tall when I'm young, I'm shorty when I'm old. What am I?"
          line2 <- getLine
          if (line2 `elem` ["pencil"])
            then do 
              putStrLn ""
              putStrLn "LAST QUESTION: what belongs to you but others use it more than you do?"
              line3 <- getLine
              if (line3 `elem` ["name"])
                then do
                  putStrLn ""
                  putStrLn "Well done, you have opened the gate."
                  return (life)
                else do 
                  putStrLn "DEAD at third Q3."
                  update_state 0 (life)
            else do 
              putStrLn "DEAD at second Q2."
              update_state 0 (life)
        else do 
            putStrLn "DEAD at first Q1."
            update_state 0 (life)

main = real_game 1

-- third_stage :: StrangerState -> IO StrangerState
-- third_stage (life) = 
--   do 
--     putStrLn "This is the last stage. You make this through, you win"
--     runGame1 
--     return (life)