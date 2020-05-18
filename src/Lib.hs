module Lib
    ( getYesOrNo
    ,getCrystalBallAnswer
    ,rollDice
    ) where

import System.Random

getYesOrNo :: Float -> IO String
getYesOrNo prob = do
  let max = 1000000
  randomNum <- getStdRandom (randomR (0, max))
  let answer = if randomNum < max * prob then "yes" else "no"
  return answer

getCrystalBallAnswer :: IO String
getCrystalBallAnswer = do
  let answers = ["It is certain",
               "It is dicidedly so",
               "Without a doubt",
               "Yes – definitely",
               "You may rely on it",
               "As I see it, yes",
               "Most likely",
               "Outlook good",
               "Yes",
               "Signs point to yes",
               "Reply hazy, try again",
               "Ask again later",
               "Better not tell you now",
               "Cannot predict now",
               "Concentrate and ask again",
               "Don't count on it",
               "My reply is no",
               "My sources say no",
               "Outlook not so good",
               "Very doubtful"]
  a <- getStdRandom(randomR (0 :: Double, 1))
  return $ answers !! (helper a 1 (toEnum $ length answers))
    where
      helper val num leng = if val - 1/leng <= 0
                            then num-1
                            else helper (val - 1/leng) (num + 1) leng

distribution :: Int -> Double
distribution num = [0.1,
                    0.15,
                    0.09,
                    0.23,
                    0.3,
                    0.13] !! (num - 1)

rollDice :: IO Int
rollDice = do
  a <- getStdRandom(randomR (0 :: Double, 1))
  return $ helper a 1
    where
      helper randNum val = if randNum - (distribution val) <= 0
                           then val
                           else helper (randNum - (distribution val)) (val + 1)
