{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

{-# OPTIONS_GHC -fno-cse #-}

module Main  where

import Types
import qualified Gpt as G
import Extra

import System.Console.CmdArgs
import System.Environment (getArgs)
import System.Process
import System.Console.ANSI
import System.Exit

import Data.Aeson as A
import Control.Monad.Trans.RWS 

main :: IO ()
main = loadConfigAndRun  mainAG

 
loadConfigAndRun :: (AGEnv  -> IO ()) -> IO ()
loadConfigAndRun mAG = do
  args <- cmdArgs readArgs
  fPGpt <- check_promt "f"
  rPGpt <- check_promt "r"
  agda <- check_agda (agda args)
  conf <- check_config (conF args)
  config  <- (A.decodeFileStrict conf) :: IO (Maybe FromConfig)
  let md = mode args
  case config of
    Nothing -> do
     cPrint  ("\nConfig file seems to be incorrect check it:  \n" ++ conf)  Red
     putStrLn "--"
     die "Something went wrong, try one more time"
    Just c ->
             let 
             m = case md of
                      "Pretty" -> PrettyMode
                      _        -> DebugMode
                 
             env = AGEnv
               { apiKey = gptApiKey c
               , agdaFileName = agda
               , taskDescription = (task args)
               , operationMode = m
               , maxTurns = maxT args
               , fGptTemp = fPGpt
               , rGptTemp = rPGpt
               , gptModel =  gpt_model c
               }
            in mAG env
             

mainAG :: AGEnv -> IO ()
mainAG env = do
  checkAgdaF <- G.tryToCompile $  (agdaFileName env)
  case checkAgdaF of
    Just x -> do
       cPrint  ("Incorrect  agda File:  " ++ (agdaFileName env) ++ "\n\n" ++ "COMPILER ERROR: " ++ x ) Red 
    Nothing -> do
               initInfo env
               conversation env []

conversation :: AGEnv -> [ConvPart] -> IO ()
conversation env cP = do
  (mValue, state, _ ) <- runRWST G.debugMode env cP
  let l = length state
  case mValue of
    Just x -> 
      if l  < (maxTurns env)
      then
        do
          conversation env state
        else do
        cPrint "Too many attempts, GPT-Agda fail. Increase max turn or change agda task for GPT." Red

    Nothing ->do
      setSGR [(SetColor Foreground Dull Green)]
      putStrLn $ "Compilation succeeded in " ++ (show l) ++ " attempts. Check new agda File" 
      setSGR [Reset]

initInfo :: AGEnv ->  IO ()
initInfo env = do
  setSGR [(SetColor Foreground Dull Blue)]
  putStrLn "\n\n\n###############################################"
  putStrLn "Started with the following data:\n\n"
  setSGR [Reset]
  putStrLn $"TASK:  " ++ (taskDescription env) ++ "\n\n"
  putStrLn $"MODE:  " ++ (show (operationMode env)) ++ "\n\n"
  putStrLn $"MAX TURN :  " ++ (show (maxTurns env)) ++ "\n\n"
  putStrLn $"MODEL:  " ++ (gptModel env) ++ "\n\n"
  agdaFile <- readFile  (agdaFileName env)
  setSGR [(SetConsoleIntensity BoldIntensity)]
  putStrLn "AGDA_CODE: \n\n" 
  setSGR [(Reset)]
  putStrLn agdaFile
  case operationMode env of
    PrettyMode -> do
      setCursorPosition 0 0
      clearScreen
    DebugMode -> return ()


-- main :: IO ()
-- main = putStrLn "hello"


 


-- loadConfigAndRun mAG = do
--   args <- getArgs
--   let (x1:x2:x3:x4:x5:_) = args
--   case length (args :: [String]) of
--     5 -> do
--          config  <- (A.decodeFileStrict x5) :: IO (Maybe FromConfig)
--          case config of
--            Nothing -> do
--              cPrint  "\nThere is something wrong with config file check it out:  \n" Red
--              putStrLn $ x5 ++ "\n"
--            Just c ->
--              let 
--              mode = case x3 of
--                       "Pretty" -> PrettyMode
--                       _        -> DebugMode
                 
--              env = AGEnv
--                { apiKey = gptApiKey c
--                , agdaFileName = x1
--                , agdaFilesDir = pathAgdaFileDir c
--                , taskDescription = x2
--                , dbCredentials = "empty"
--                , operationMode = mode
--                , maxTurns = (read x4) :: Int
--                , fGptTemp = f_GptTemp c
--                , rGptTemp = r_GptTemp c
--                , gptModel =  gpt_model c
--                }
--             in mAG env
             
--     _ -> do

--       cPrint "Incorrect number of parameters passed during startup" Red
