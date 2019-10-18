module RPS where

import System.Exit
import Data.Char  (isSpace, toUpper)
import Data.Maybe (isNothing)
type StrangerState = (Int)

data Move = Rock | Paper | Scissors
  deriving (Show, Eq, Enum)

instance Ord Move where
  (<=) x y = x == y || elem (x, y) pairs
    where
      pairs = [(Rock, Paper), (Paper, Scissors), (Scissors, Rock)]

data Outcome = Win | Loss | Draw
  deriving (Show, Eq)

update_state :: Integer -> StrangerState -> IO StrangerState
update_state val (life)
  | val == 0 = do
      return (life-1)
      die "GAME OVER"
  | otherwise = do
      putStrLn "" 
      return (life)

getOutcome :: Move -> Move -> Outcome
getOutcome user computer =
  if user >= computer
    then Win
    else Loss

-- Prompt the user to input a move
getUserInput :: IO String
getUserInput = do
  putStr "What will you pick? "
  getLine

-- Parse input and maybe return a move
parseInput :: String -> Maybe Move
parseInput string = case filter (not . isSpace) (map toUpper string) of
  "ROCK"     -> Just Rock
  "PAPER"    -> Just Paper
  "SCISSORS" -> Just Scissors
  _          -> Nothing

-- Prompt the user until they enter a valid move
getUserMove :: IO (Move)
getUserMove = do
  maybeMove <- parseInput <$> getUserInput
  case maybeMove of
    Nothing -> do
      putStrLn "Please enter a valid move."
      getUserMove
    (Just move) -> return move

runGame :: Maybe Move -> Maybe Outcome -> StrangerState -> IO StrangerState
runGame (Just move) Nothing (life) =
  do
    userMove <- getUserMove
    let outcome = getOutcome userMove move
    if outcome == Win
      then do
        putStrLn "You won the match."
        update_state 1 (life)
      else do
      putStrLn "You lost the match."
      update_state 0 (life)