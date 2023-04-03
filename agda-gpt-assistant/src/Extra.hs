{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Extra where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Trans.RWS 
import Control.Monad.IO.Class (liftIO)
import Data.Maybe

import  Types

import System.Environment (getEnv)

import System.Directory
import Data.Aeson as A 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Process as SP
import qualified Data.List as L
import Data.List.Split as DLS(splitOn)


import Data.Time.Clock.POSIX
import Data.Time.Format 


import System.Console.ANSI


import Data.List.Split 


import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Text as T
import System.Exit
import System.FilePath (splitFileName)

cPrint :: String -> Color -> IO ()
cPrint s c = do
  setSGR [(SetColor Foreground Dull c )]
  putStrLn s
  setSGR [Reset]


  
cpFile :: AGEnv -> IO ()
cpFile env = do
  system $ "cp "  ++ (agdaFile env) ++ " "  ++ (agdaFile env) ++ ".bak"
  return ()

cpAFile :: AGEnv -> IO ()
cpAFile env = do
  system $ "cp " ++ (agdaFile env) ++ ".bak" ++ " " ++ (agdaFile env)
  return ()

rmAFile :: AGEnv -> IO ()
rmAFile env = do
  system $ "rm " ++  (agdaFile env)
  return ()


check_promt :: String ->  IO String
check_promt s = do
  home <- getEnv "HOME"
  l <- doesFileExist $ "./templates/gpt_"++s++".txt"
  case l of
    True -> return $ "./templates/gpt_"++s++".txt"
    False -> do
      g <- doesFileExist $ home ++ "/.agda-gpt-assistant/templates/gpt_"++s++".txt"
      case g of
        True -> return  $ home++"/.agda-gpt-assistant/templates/gpt_"++s++".txt"
        False -> do
          cPrint ("I can't find gpt_"++s++".txt, check it in /templates/ or ~/.agda-gpt-assistant/templates\n") Red
          putStrLn "--"
          die "Something went wrong, try one more time"

check_agda :: String ->  IO String
check_agda file = do
  l <- doesFileExist $ file
  case l of
    True -> return file
    False -> do
       cPrint ("I can't find agda file: "++ file ) Red
       putStrLn "--"
       die "Something went wrong, try one more time"


check_config :: String -> IO String
check_config conf = do
  home <- getEnv "HOME"
  l <- doesFileExist $ conf
  case l of
    True -> return $ conf
    False -> do
      g <- doesFileExist $ home++"/.agda-gpt-assistant/" ++ conf 
      case g of
        True -> return  $ home++"/.agda-gpt-assistant/"++ conf
        False -> do
          cPrint ("I can't find " ++ conf ++ ", check it in currnet dir or ~/.agda-gpt-assistant/\n") Red
          putStrLn "--"
          die "Something went wrong, try one more time"



timestamp :: IO String
timestamp = do
  current <- getPOSIXTime
  let formatted = formatTime defaultTimeLocale "%H:%M:%S" (posixSecondsToUTCTime current)
  return formatted
