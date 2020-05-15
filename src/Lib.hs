module Lib
    ( getYesOrNo
    ) where

import System.Random

getYesOrNo :: Float -> IO String
getYesOrNo prob = do
  let max = 1000000
  randomNum <- getStdRandom (randomR (0, max))
  let answer = if randomNum < max * prob then "yes" else "no"
  return answer
