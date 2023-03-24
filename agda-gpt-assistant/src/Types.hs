{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Types  where

import Data.Aeson as A

import Control.Monad.IO.Class

import Control.Monad.Trans.RWS
import Control.Monad.RWS.Class


data OperationMode =  PrettyMode |  DebugMode
  deriving (Show)


data AGEnv = AGEnv
    { apiKey :: String
    , agdaFileName :: FilePath
    , agdaFilesDir :: FilePath
    , agdaCompilerPath :: FilePath
    , taskDescription :: String
    , dbCredentials :: String
    , operationMode :: OperationMode
    , maxTurns :: Int
    , fGptTemp :: FilePath
    , rGptTemp :: FilePath
    } deriving (Show)

-- data  

data FromConfig = FromConfig
  { gptApiKey :: String
  , pathAgdaFileDir :: String
  , pathAgdaCompiler :: String
  , f_GptTemp :: FilePath
  , r_GptTemp :: FilePath
  } deriving (Show)

instance FromJSON FromConfig where
  parseJSON = withObject "Config" $ \v -> FromConfig
    <$> v .: "GPT_Api_key"
    <*> v .: "path_agda_file_dir"
    <*> v .: "Path_agda_compiler"
    <*> v .: "first_template_to_gpt"
    <*> v .: "rest_template_to_gpt" 






data ConvPart = ConvPart
                  { gpt_input :: String
                  , gpt_res :: String
                  , file_state :: String
                  , agda_res :: Maybe String
                  } deriving (Show)


type AGMonad  = RWST AGEnv () [ConvPart] IO  
