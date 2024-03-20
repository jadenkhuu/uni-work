{-# LANGUAGE FlexibleContexts #-}
module Ex06 where

import Control.Monad.State
import System.IO
import System.Random
import Test.QuickCheck
import Text.Read (readMaybe)

import Data.List (nub, sort)


-- TASK 1 --

-- Implement an IO procedure that reads the given input
-- file, throws away all the lines in the file that
-- appear more than once, and writes the remaining
-- (unique) lines to the given output file.
--
-- You will want to use the `readFile` and `writeFile`
-- procedures.

onlyUnique :: FilePath -> FilePath -> IO ()
onlyUnique inputFile outputFile = do
  contents <- readFile inputFile
  let unique = nub (lines contents)
  writeFile outputFile (unlines unique)

-- TASK 2 --

-- There is a well-known guessing game where the player
-- attempts to guess a secret integer number that has been chosen
-- randomly between 1 and 100.

-- If the player guesses incorrectly, the only information
-- they receive is whether the secret number is lower or
-- higher than the player's guess.

-- The player wins if they successfully guess the secret.
-- If the player fails to guess the secret number in a certain
-- number of turns, they lose.

-- We'll implement this game in Haskell below. Your task
-- will be to develop and test an AI player for this game.


data GameState =
  GameState {
    remainingTurns :: Int,
    secretNumber :: Int
  }
data Outcome = PlayerWon | PlayerLost deriving (Eq, Show)
data Response = Lower | Higher deriving (Eq, Show)

-- Player implementations will be modelled using two
-- "callback" functions: `guess`, which returns the player's
-- next guess, and `signalWrong`, which lets the player know
-- the response (lower or higher) to their last guess.

-- Some players, such as humans, will work in the IO monad,
-- since to get their guess, we need to do input/output.
-- The AI player will work in the State monad: it will make
-- its `guess` using a deterministic knowledge state, and it
-- will update its knowledge state based on responses.
data Player m =
  Player {
    guess :: m Int,
    signalWrong :: Response -> m ()
  }


-- The gameLoop can work in any monad, including m = IO.
gameLoop :: (Monad m) => GameState -> Player m -> m Outcome
gameLoop (GameState 0 _) player = return PlayerLost
gameLoop (GameState r s) player = do
  g <- guess player
  case compare s g of
    LT -> do
      signalWrong player Lower
      gameLoop (GameState (r-1) s) player
    GT -> do
      signalWrong player Higher
      gameLoop (GameState (r-1) s) player
    EQ -> return PlayerWon

-- The Human Player:

human :: Player IO
human = Player { guess = getGuess, signalWrong = printWrong }
  where
    getGuess = do
      putStr "Enter a number (1-100): "
      x <- getLine
      case readMaybe x of
        Nothing -> getGuess
        Just i  -> return i
    printWrong Lower  = putStrLn "Too high!"
    printWrong Higher = putStrLn "Too low!"

play :: IO ()
play = do
  putStrLn "I chose a random number between 1 and 100."
  putStrLn "You have five turns to guess it!"
  s <- randomRIO (1,100)
  outcome <- gameLoop (GameState 5 s) human
  case outcome of
    PlayerWon  -> putStrLn "You got it!"
    PlayerLost -> do
      putStrLn "You ran out of guesses!"
      putStrLn $ "The number was " ++ show s ++ "."


-- The AI player:

data AiState =
  AiState {
    lowerBound :: Int,
    upperBound :: Int
  } deriving (Show, Eq)


ai :: Player (State AiState)
ai = Player { guess = aiGuess, signalWrong = aiWrong } where
  aiGuess = do
    AiState lower upper <- get
    let mid = (lower + upper) `div` 2
    return mid

  aiWrong response = do
    AiState lower upper <- get
    case response of
      Lower -> put $ AiState lower (upper - 1)
      Higher -> put $ AiState (lower + 1) upper

-- Testing the AI:

-- A wrapper class so that QuickCheck can
-- generate secret numbers between 1 and 100.
data Secret = Secret Int deriving (Show)
instance Arbitrary Secret where
  arbitrary = Secret <$> choose (1,100)

-- any working AI should be able to win in 100 guesses
prop_winsEventually (Secret s) =
  evalState (gameLoop (GameState 100 s) ai) (AiState 1 100) == PlayerWon

-- a perfect AI can win any game in at most 7 guesses
prop_winsOptimally (Secret s) =
  evalState (gameLoop (GameState 7 s) ai) (AiState 1 100) == PlayerWon

