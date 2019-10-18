module GUN where

import System.Exit
import Data.Char  (isSpace, toUpper)
import Data.Maybe (isNothing)
import System.IO
import Text.Read   (readMaybe)
import Data.Maybe   (fromJust)
import RPS

data Moves = Charge | Attack | Block | Special 
  deriving (Show, Eq)

-- data Outcome = Win | Loss | Tie
--   deriving (Show, Eq)

 -- StrangerState here means: if 0 then not charged and can't attack, if 1 the user/computer may attack 
-- getOutcomes1 :: Moves -> Integer -> Moves
-- getOutcomes1 user count =
--   if (count == 0)
--     then 
--       putStrLn "must CHARGE"
--       return user
--     else if user == Charge
--       then 
--         (count+1)
--         return user
--       else return user 

getOutcomes :: Moves -> Moves -> StrangerState -> Outcome 
getOutcomes user computer charge 
   | charge == 0 && user == Charge && computer == Charge = Draw
   | charge == 0 && user == Charge && computer == Attack = Loss
   | charge == 0 && user == Charge && computer == Block = Draw
   | charge == 0 && user == Charge && computer == Special = Loss
   | charge == 0 && user == Attack && computer == Charge = Win
   | charge == 0 && user == Attack && computer == Attack = Draw
   | charge == 0 && user == Attack && computer == Block = Draw
   | charge == 0 && user == Attack && computer == Special = Draw
   | charge == 0 && user == Block && computer == Charge = Draw
   | charge == 0 && user == Block && computer == Attack = Loss
   | charge == 0 && user == Block && computer == Block = Draw
   | charge == 0 && user == Block && computer == Special = Win
   | charge == 0 && user == Special && computer == Charge = Win
   | charge == 0 && user == Special && computer == Attack = Win
   | charge == 0 && user == Special && computer == Block = Win
   | charge == 0 && user == Special && computer == Special = Win
   | otherwise = Draw

-- Prompt the user to input a move
getUserInputs :: IO String
getUserInputs = do
  putStr "What will you pick? "
  getLine

-- Parse input and maybe return a move
parseInputs :: String -> Maybe Moves
parseInputs string = case filter (not . isSpace) (map toUpper string) of
  "CHARGE"   -> Just Charge
  "ATTACK"   -> Just Attack
  "BLOCK"    -> Just Block
  "SPECIAL"  -> Just Special
  _          -> Nothing

-- Prompt the user until they enter a valid move
getUserMoves :: IO (Moves)
getUserMoves = do
  maybeMoves <- parseInputs <$> getUserInputs
  case maybeMoves of
    Nothing -> do
      putStrLn "Please enter a valid move."
      getUserMoves
    (Just moves) -> return moves

playGame :: Moves -> StrangerState -> IO StrangerState
playGame user charge =
  do
    userMove <- getUserMoves
    let outcome = getOutcomes userMove user charge
    if outcome == Win
      then do
        putStrLn "You won the match."
        update_state 1 charge
      else if outcome == Draw
        then do 
          putStrLn "Tied, go again"
          playGame user charge
      else do
      putStrLn "You lost the match."
      update_state 0 charge

-- run = playGame 

