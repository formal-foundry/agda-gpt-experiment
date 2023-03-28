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


data ChatCompletion = ChatCompletion
    { id :: String
    , object :: String
    , created :: Integer
    , model :: String
    , usage :: Usage
    , choices :: [Choice]
    } deriving (Show)

data Usage = Usage
    { prompt_tokens :: Int
    , completion_tokens :: Int
    , total_tokens :: Int
    } deriving (Show)

data Choice = Choice
    { message :: Message
    , finish_reason :: String
    , index :: Int
    } deriving (Show)

data Message = Message
    { role :: String
    , content :: String
    } deriving (Show)

instance FromJSON ChatCompletion where
    parseJSON = withObject "ChatCompletion" $ \v -> ChatCompletion
        <$> v .: "id"
        <*> v .: "object"
        <*> v .: "created"
        <*> v .: "model"
        <*> v .: "usage"
        <*> v .: "choices"

instance FromJSON Usage where
    parseJSON = withObject "Usage" $ \v -> Usage
        <$> v .: "prompt_tokens"
        <*> v .: "completion_tokens"
        <*> v .: "total_tokens"

instance FromJSON Choice where
    parseJSON = withObject "Choice" $ \v -> Choice
        <$> v .: "message"
        <*> v .: "finish_reason"
        <*> v .: "index"

instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> Message
        <$> v .: "role"
        <*> v .: "content"

instance ToJSON Message where
  toJSON (Message role content) = A.object ["role" .= role, "content" .= content]




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
    , gptModel :: String
    } deriving (Show)

-- data  

data FromConfig = FromConfig
  { gptApiKey :: String
  , pathAgdaFileDir :: String
  , pathAgdaCompiler :: String
  , f_GptTemp :: FilePath
  , r_GptTemp :: FilePath
  , gpt_model :: String
  } deriving (Show)

instance FromJSON FromConfig where
  parseJSON = withObject "Config" $ \v -> FromConfig
    <$> v .: "GPT_Api_key"
    <*> v .: "path_agda_file_dir"
    <*> v .: "Path_agda_compiler"
    <*> v .: "first_template_to_gpt"
    <*> v .: "rest_template_to_gpt"
    <*> v .: "gpt_model"






data ConvPart = ConvPart
                  { gpt_input :: String
                  , gpt_res :: String
                  , pure_code_res :: String
                  , current_agad_file :: String
                  , agda_res :: Maybe String
                  , promptL :: [Message]
                  } deriving (Show)


type AGMonad  = RWST AGEnv () [ConvPart] IO  
