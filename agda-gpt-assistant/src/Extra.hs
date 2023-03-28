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
import Data.Aeson as A 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Process as SP
import qualified Data.List as L
import Data.List.Split as DLS(splitOn)

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
  system $ "cp " ++(agdaFilesDir env ) ++ (agdaFileName env) ++ " "
       ++ (agdaFilesDir env ) ++ "bak/"  ++ (agdaFileName env)++ ".bak"
  return ()

cpAFile :: AGEnv -> IO ()
cpAFile env = do
  system $ "cp " ++(agdaFilesDir env )  ++ "bak/" ++ (agdaFileName env) ++ ".bak" ++ " "
       ++ (agdaFilesDir env )  ++ (agdaFileName env)
  return ()

rmAFile :: AGEnv -> IO ()
rmAFile env = do
  system $ "rm " ++ (agdaFilesDir env )   ++ (agdaFileName env)
  return ()


