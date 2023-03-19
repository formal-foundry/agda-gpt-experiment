{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class as M
import qualified Gpt as G 
import Web.Scotty as S
import Data.Text (pack)
import Data.Text.Lazy (fromStrict)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

-- main :: IO ()
-- main = do
--   putStrLn "Enter a prompt for the GPT API:"
--   prompt <- getLine
--   G.interactWithGpt prompt

main :: IO ()
main = scotty 3000 $ do
    post "/askme" $ do
      requestBody <- body
      res <- M.liftIO $ G.interactWithGpt $ BS.unpack requestBody 
      text $ fromStrict $ pack res

    -- post "/hint" $ do
    --   hjs <- body
     
--  Checking Bad (/home/kryn/agda-gpt-assistant/agda-gpt-assistant/data/Bad.agda).
-- /home/kryn/agda-gpt-assistant/agda-gpt-assistant/data/Bad.agda:8,9-11
-- A should be a function type, but it isn't
-- when checking that a1 is a valid argument to a function of type A

--  Checking Bad (Bad.agda).
-- Bad.agda:8,9-11
-- A should be a function type, but it isn't
-- when checking that a1 is a valid argument to a function of type A
