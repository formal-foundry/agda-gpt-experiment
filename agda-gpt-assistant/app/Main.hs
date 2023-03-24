{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Main  where

import Types
import System.Environment (getArgs)

import System.Console.ANSI

import Data.Aeson as A

import System.Console.Haskeline

import Control.Monad.IO.Class as M
import qualified Gpt as G 
import Web.Scotty as S
import Data.Text (pack)
import Data.Text.Lazy (fromStrict)

import Control.Monad.Trans.RWS 
import Control.Monad.IO.Class (liftIO)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = loadConfigAndRun  mainAG

 
loadConfigAndRun :: (AGEnv  -> IO ()) -> IO ()
loadConfigAndRun mAG = do
  args <- getArgs
  let (x1:x2:x3:x4:x5:_) = args
  case length (args :: [String]) of
    5 -> do
         config  <- (A.decodeFileStrict x5) :: IO (Maybe FromConfig)
         case config of
           Nothing -> putStrLn "Bad config file"
           Just c ->
             let 
             mode = case x3 of
                      "pretty" -> PrettyMode
                      _        -> DebugMode


             env = AGEnv
               { apiKey = gptApiKey c
               , agdaFileName = x1
               , agdaFilesDir = pathAgdaFileDir c
               , agdaCompilerPath = pathAgdaCompiler c
               , taskDescription = x2
               , dbCredentials = "empty"
               , operationMode = mode
               , maxTurns = (read x4) :: Int
               , fGptTemp = f_GptTemp c
               , rGptTemp = r_GptTemp c
               }
            in mAG env
             
    _ -> putStrLn "invalid number of parameters passed"

 
-- bin [agda_file_name] [task description] [operation mode] [max turns] [config name]  

  
mainAG :: AGEnv -> IO ()
mainAG env = do
  initInfo env
  case operationMode env of
     DebugMode -> do
       (a, s, w) <- runRWST G.debugMode env []
       putStrLn a
     
     PrettyMode -> putStrLn "not ready, yet"
  


initInfo :: AGEnv ->  IO ()
initInfo env = do
  setSGR [(SetColor Foreground Dull Blue)]
  putStrLn "###############################################"
  putStrLn "Start Program with params:\n\n"
  setSGR [Reset]
  setSGR [SetConsoleIntensity BoldIntensity]
  putStrLn "param details"
  setSGR [Reset]
  putStrLn "Details......."

www :: Int -> IO ()
www k = do
  putStrLn "x"
  if k > 0
  then www ( k - 1)
  else return ()
