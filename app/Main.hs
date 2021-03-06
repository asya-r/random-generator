module Main where

import Data.Semigroup ((<>))
import Lib
import Options.Applicative

data Options = Options
  { mode :: Int
  , probability :: Float}

options :: Parser Options
options = Options
      <$> option auto
          ( long "mode"
         <> metavar "MODE"
         <> help "1 for yes-no, 2 for crystal ball, 3 for dice game" )
      <*> option auto
          ( long "probability"
         <> short 'p'
         <> showDefault
         <> value 0.5
         <> help "Probability for 1 mode" )


main :: IO ()
main = greet =<< execParser opts
 where
   opts = info (options <**> helper)
     ( fullDesc
    <> progDesc "Print MODE (1 for yes-no, 2 for crystal ball, 3 for dice game)" )

greet :: Options -> IO ()
greet (Options 1 p) = do
  rand <- getYesOrNo p
  print rand
greet (Options 2 _) = do
  rand <- getCrystalBallAnswer
  print rand
greet (Options 3 _) = do
  rand <- rollDice
  print rand
greet _ = return ()
