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
import System.Process

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
  checkAgdaF <- G.tryToCompile $ (agdaFilesDir env ) ++ (agdaFileName env)
  case checkAgdaF of
    Just x -> do
       setSGR [(SetColor Foreground Dull Red)]
       putStrLn $ "bad agda File" ++ x 
       setSGR [Reset]
    Nothing -> case operationMode env of
                 DebugMode -> xx env []
                                           
                 PrettyMode -> putStrLn "not ready, yet"

xx :: AGEnv -> [ConvPart] -> IO ()
xx env cP = do
  cpFile env
  putStrLn "Kopiuje plik agdy do katalogu bak\n\n"
  putStrLn "przed odpaleniem runRWST"
  (mValue, state, _ ) <- runRWST G.debugMode env cP
  putStrLn "powrót do XX Main po odpaleniu runRSt\n\n"
  putStrLn "usuwam plik agdy po zmianach\n\n"
  rmAFile env
  putStrLn "kopiuję z bal plik agdy \n\n"
  cpAFile env 
  putStrLn "usuwam plik bak agdy \n\n"
  rmBFile env
  putStrLn " po usunięciu pliku bak"
  case mValue of
    Just x -> 
      if length state < 3
      then
        do
          putStrLn $ "sprawdzam dlugośc lista ze stanem w main xx:   " ++ show ( length state)  ++ "\n\n"
          putStrLn "przed odpaleniem kolejny raz funkcjixx funckji\n\n"
          xx env state
        else putStrLn "f"
    Nothing -> putStrLn "kompilacja OK"  
  
cpFile :: AGEnv -> IO ()
cpFile env = do
  system $ "cp " ++(agdaFilesDir env ) ++ (agdaFileName env) ++ " "
       ++ (agdaFilesDir env ) ++ "bak/"  ++ (agdaFileName env)++ ".bak"
  return ()

cpAFile :: AGEnv -> IO ()
cpAFile env = do
  system $ "cp " ++(agdaFilesDir env )  ++ "bak/" ++ (agdaFileName env) ++ ".bak" ++ " "
       ++ (agdaFilesDir env )  ++ (agdaFileName env)
  return ()

rmBFile :: AGEnv -> IO ()
rmBFile env = do
  system $ "rm " ++ (agdaFilesDir env ) ++ "bak/"  ++ (agdaFileName env)++ ".bak"
  return ()

rmAFile :: AGEnv -> IO ()
rmAFile env = do
  system $ "rm " ++ (agdaFilesDir env )   ++ (agdaFileName env)
  return ()

catFile :: AGEnv -> IO ()
catFile env = do
  system $ "cat " ++(agdaFilesDir env ) ++ "bak/" ++ (agdaFileName env) ++ ".bak" ++ " " ++ (agdaFilesDir env )  ++ (agdaFileName env)
  return ()


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
