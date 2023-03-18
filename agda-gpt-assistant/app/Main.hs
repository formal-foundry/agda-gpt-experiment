module Main (main) where

import qualified Gpt as G 


main :: IO ()
main = do
  putStrLn "Enter a prompt for the GPT API:"
  prompt <- getLine
  G.interactWithGpt prompt
