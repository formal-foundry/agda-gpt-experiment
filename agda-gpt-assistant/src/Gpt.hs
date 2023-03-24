

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Gpt where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Trans.RWS 
import Control.Monad.IO.Class (liftIO)


import  Types 
import Data.Aeson as A 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Process as SP
import qualified Data.List as L
import Data.List.Split as DLS(splitOn)



import Data.List.Split 
import Text.Regex.Posix as RP 


import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Text as T
import System.Exit
import System.FilePath (splitFileName)


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




decodeRes :: BL.ByteString -> String
decodeRes r = case ((A.decode r ):: Maybe ChatCompletion ) of
                     Nothing -> "Nothing"
                     Just x -> content $ message $ Prelude.head $ choices x


encodeReq :: String -> BL.ByteString
encodeReq msg = encode $
    A.object [ "model" .= ("gpt-3.5-turbo" :: String)
           , "messages" .= [ A.object [ "role" .= ("user" :: String)
                                     , "content" .= msg
                                     ]
                           ]
           ]


-- GPT API endpoint
gptApiUrl :: String
gptApiUrl = "https://api.openai.com/v1/chat/completions"

-- Your API key
apiKey_ :: B.ByteString
apiKey_ = "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"

-- Create a request to the GPT API
createGptRequest :: String -> Request
createGptRequest prompt = request
  where
    baseRequest = parseRequest_ gptApiUrl
    request = baseRequest
      { method = "POST"
      , requestHeaders = [ ("Content-Type", "application/json")
                         , ("Authorization", B.concat ["Bearer ", apiKey_])
                         ]
      , requestBody = RequestBodyLBS (encodeReq prompt)
      }

interactWithGpt :: String -> IO String
interactWithGpt prompt = do
  manager <- newManager tlsManagerSettings
  request <- return (createGptRequest prompt)
  response <- httpLbs request manager
  -- putStrLn $ "Status: " ++ (show $ responseStatus response)
  return  $ (decodeRes $ responseBody response)


tryToCompile :: FilePath -> IO (Maybe String)
tryToCompile fp = do
  let (path, file) =  splitFileName fp
  aReq <- runProcess_ path file
  let ret = case aReq of
              Nothing -> Nothing
              Just re -> Just $ removeErrorSubstring path $ removeErrorSubstring path re
  return ret 

                 
removeErrorSubstring :: String -> String -> String
removeErrorSubstring substr str = go str
  where go [] = []
        go s@(x:xs)
          | substr `L.isPrefixOf` s = L.drop (L.length substr) s
          | otherwise = x : go xs


runProcess_ ::  FilePath -> String -> IO (Maybe String)
runProcess_ pwd afile = do
  let cp = shell $ "/home/kryn/.cabal/bin/agda" ++ " " ++ afile
      ncp = cp { cwd = Just pwd
               , std_out = CreatePipe
               , std_err = CreatePipe
               }            
  (code, output, errorOutput) <- readCreateProcessWithExitCode ncp ""
  let result = case code of
                  ExitSuccess   -> Nothing
                  ExitFailure _ -> Just output
  return result
  

extractCodeBlocksFromGPT :: String -> [String]
extractCodeBlocksFromGPT str = extractCodeBlocks' str []
  where extractCodeBlocks' [] acc = acc
        extractCodeBlocks' xs acc = let block = takeCodeBlock xs
                                    in case block of
                                      Nothing -> extractCodeBlocks' (L.drop 1 xs) acc
                                      Just (code, rest) -> extractCodeBlocks' rest (acc ++ [code])

        takeCodeBlock :: String -> Maybe (String, String)
        takeCodeBlock xs = if L.take 3 xs == "```"
                           then let (block, rest) = L.span (/= '`') (L.drop 3 xs)
                                in Just (block, L.drop 3 rest)
                           else Nothing





debugMode :: AGMonad String
debugMode = do
  env <- ask
  liftIO $ putStrLn "helllo frm debug"
  liftIO $ firstConv env
  return "fde"




firstConv :: AGEnv -> IO ()
firstConv env= do
    x <- readFile $ fGptTemp env
    putStrLn x
    putStrLn "e"


replaceText :: String -> String -> String -> String
replaceText [] _ _ = []
replaceText str search replace
  | L.take (L.length search) str == search = replace ++ replaceText (L.drop (L.length search) str) search replace
  | otherwise = L.head str : replaceText (L.tail str) search replace

  -- ConvPart


-- ____________ TEST____________

-- main :: IO ()
-- main = do
--   contents <- readFile "file.txt"
--   putStrLn contents



x :: IO ()
x = do
  x <-tryToCompile  "/home/kryn/agda-gpt-assistant/agda-gpt-assistant/data/A.agda"
  let s =  case x of
           Nothing -> "OK"
           Just k -> k
  putStrLn s


x2 :: IO ()
x2 = do
  x <-tryToCompile  "/home/kryn/agda-gpt-assistant/agda-gpt-assistant/data/Bad.agda"
  let s =  case x of
           Nothing -> "OK"
           Just k -> k
  putStrLn s




test2 :: String 
test2 = "\n\nThe implementation of the `add` function can be done recursively by pattern matching on the first argument `n`.\n\nHere's the complete Agda code implementing this function:\n\n```\ndata Nat : Set where\n  zero : Nat\n  suc : Nat → Nat\n\nadd : Nat → Nat → Nat\nadd zero m = m\nadd (suc n) m = suc (add n m)\n```\n\nThe first equation of the `add` function defines the base case for the recursion, where adding zero to any number `m` has no effect and returns `m`.\n\nThe second equation handles the recursive case where we have a successor `suc n` of some number `n`. To add this number to another number `m`, we simply recursively add `n` to `m`, and then add one more to the result with a `suc` constructor."

