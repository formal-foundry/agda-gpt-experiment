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
-- import Text.Regex.Posix as RP 


import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Text as T
import System.Exit
import System.FilePath (splitFileName)



decodeRes :: BL.ByteString -> String
decodeRes r = case ((A.decode r ):: Maybe ChatCompletion ) of
                     Nothing -> "Nothing"
                     Just x -> content $ message $ Prelude.head $ choices x

genJsonReq :: [Message] -> BL.ByteString
genJsonReq messages =
  let gptModel = ("gpt-3.5-turbo" :: String)
  in 
  encode $ A.object ["model" .= gptModel, "messages" .= (L.reverse messages)]


-- encodeReq :: String -> BL.ByteString
-- encodeReq msg = encode $
--     A.object [ "model" .= ("gpt-3.5-turbo" :: String)
--            , "messages" .= [ A.object [ "role" .= ("user" :: String)
--                                      , "content" .= msg
--                                      ]
--                            ]
--            ]

-- Create a request to the GPT API
createGptRequest :: [Message] -> String -> Request
createGptRequest prompt key = request
  where
    apiKey_ = B.pack key 
    baseRequest = parseRequest_ "https://api.openai.com/v1/chat/completions"
    request = baseRequest
      { method = "POST"
      , requestHeaders = [ ("Content-Type", "application/json")
                         , ("Authorization", B.concat ["Bearer ", apiKey_])
                         ]
      -- , requestBody = RequestBodyLBS (encodeReq prompt)
      , requestBody = RequestBodyLBS (genJsonReq prompt)
      }

-- qwe :: [Message]
-- qwe = return Message { role = "user"
--                      , content = "who is the best nba player?"
--                      }

gptConv :: [Message] -> String -> IO (String, String)
gptConv prompt key = do
  manager <- newManager tlsManagerSettings
  request <- return (createGptRequest prompt key)
  putStrLn $show request
  response <- httpLbs request manager
  putStrLn "odpowiedz od servera gpt \n\n\n"
  putStrLn $ show $ (decodeRes $ responseBody response)
  putStrLn $ "koniec odpowiedzi\n\n"
  putStrLn $ "cały json z srwera:: ####\n\n"
  putStrLn $ show response 
  return $  (plainCode  (decodeRes $ responseBody response),
              (  decodeRes $ responseBody response))


plainCode :: String -> String
plainCode res = rmSubS "Agda" (rmSubS "agda" (L.concat $ extractCode  res))


extractCode :: String -> [String]
extractCode str = extractCodeBlocks' str []
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



-- usunięcie części string                  
rmSubS :: String -> String -> String
rmSubS substr str = go str
  where go [] = []
        go s@(x:xs)
          | substr `L.isPrefixOf` s = L.drop (L.length substr) s
          | otherwise = x : go xs

-- ____________ ########## koniec połączenia z chatem GPTTT####


tryToCompile :: FilePath -> IO (Maybe String)
tryToCompile fp = do
  let (path, file) =  splitFileName fp
  aReq <- runProcess_ path file
  case aReq of
    Nothing -> return Nothing
    Just re -> do
                putStrLn "bład z funkcji complie przed usunięciem adresów \n\n"
                putStrLn re 
                return $ Just $ rmSubS path $ rmSubS path re
  
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
  
-- -----------------------------------------------
--     , agdaFileName :: FilePath
--     , agdaFilesDir :: FilePath
catFile :: AGEnv -> IO ()
catFile env = do
  system $ "cat " ++(agdaFilesDir env ) ++ "bak/" ++ (agdaFileName env) ++ ".bak" ++ " " ++ (agdaFilesDir env )  ++ (agdaFileName env)
  return ()


debugMode :: AGMonad (Maybe String)
debugMode = do
  env <- ask
  state <- get
  liftIO $ putStrLn "jestem w funkcji debugMode\n\n"
  let agdaFile = (agdaFilesDir env) ++ (agdaFileName env)
  let firstPrompt = Message {role = "system", content = "You are a helpful assistant."}
  if L.length state == 0
  then
    do
      liftIO $ putStrLn $ "sprawdzam dlugość listy s : " ++ (show (L.length state)) ++ "\n\n"
      -- liftIO $ catFile env
      liftIO $ putStrLn "tworze zapytanie do gpt \n\n"
      fcon <- liftIO $ fConvInput env
      let promptReq = Message {role = "user", content = fcon}
      liftIO $ putStrLn "takie zapytanie idzie go gpt:!!!!!!!!!!!!!!!!!"
      liftIO $ putStrLn fcon
      liftIO $ putStrLn "koniec zapytania do GPT!!!!!!!!!!!!!\n\n"
      answareFromGPT <- liftIO $ gptConv [firstPrompt, promptReq] (apiKey env)
      let promptRes = Message {role = "assistant" , content = (snd answareFromGPT)}
      liftIO $ putStrLn "odpowiedzi od GPT!!!!!!!!!\n"
      liftIO $ putStrLn $ fst answareFromGPT ++ "\n\n"
      liftIO $ putStrLn "a tak wygląda cała odpowiedz"
      liftIO $ putStrLn $ snd answareFromGPT ++ "\n\n"
      liftIO $ putStrLn "dodaje kod z odpowiedzi GPT do pliku agdy\n\n\n\n"
      liftIO $ appendFile agdaFile (fst answareFromGPT)
      liftIO $ putStrLn "tak wygląda nowy plik agdy :  \n\n"
      newAfile <- liftIO $ readFile agdaFile
      liftIO $ putStrLn (newAfile ++ "@@@koniec czytania nowego plikiu@@" ++ "\n\n") 
      compiler <- liftIO $ tryToCompile agdaFile
      liftIO $ putStrLn "tworzę nową wartośc stanu i dodaje\\n"
      let newState = (createConvPart fcon answareFromGPT newAfile compiler [firstPrompt, promptReq, promptRes]  : state)
      liftIO $ putStrLn "robię update listy ze stanem"
      put newState
      case compiler of
        Nothing -> do
                   liftIO $ putStrLn "komplilacja agdy przebiegła OK\n\n"  
                   return Nothing
        Just x -> do
                  liftIO $ putStrLn ("błað kompilatora agdy" ++ x ++ "\n\n\n") 
                  return (Just x)
  else 
    do
      liftIO $ putStrLn $ "sprawdzam dlugość listy s : " ++ (show (L.length state)) ++ "\n\n"
      rcon <- liftIO $ rConvInput env (fromJust(agda_res (L.head state)))
      liftIO $ putStrLn "takie zapytanie idzie go gpt:!!!!!!!!!!!!!!!!!"
      liftIO $ putStrLn rcon
      liftIO $ putStrLn "koniec zapytania do GPT!!!!!!!!!!!!!\n\n"
      let rPromptReq = Message {role =  "user", content = rcon}
      let sPrompt =  promptL (L.head state) 
      answareFromGPT <- liftIO $ gptConv (rPromptReq : sPrompt)  (apiKey env)
      let sPromptReq = Message {role = "assisant", content = (snd answareFromGPT)}
      liftIO $ putStrLn "odpowiedzi od GPT!!!!!!!!!\n"
      liftIO $ putStrLn $ fst answareFromGPT ++ "\n\n"
      liftIO $ putStrLn $ snd answareFromGPT ++ "\n\n"
      liftIO $ putStrLn "dodaje kod z odpowiedzi GPT do pliku agdy\n\n\n\n"
      liftIO $ appendFile agdaFile (fst answareFromGPT)
      liftIO $ putStrLn "tak wygląda nowy plik agdy :  \n\n"
      newAfile <- liftIO $ readFile agdaFile
      liftIO $ putStrLn (newAfile ++ "koniec czytania nowego plikiu" ++ "\n\n") 
      compiler <- liftIO $ tryToCompile agdaFile
      liftIO $ putStrLn "tworzę nową wartośc stanu i dodaje\\n"
      let newState = (createConvPart rcon answareFromGPT newAfile compiler (sPromptReq:rPromptReq:sPrompt)  : state)
      liftIO $ putStrLn "robię update listy ze stanem"
      put newState
      case compiler of
        Nothing -> do
                   liftIO $ putStrLn "komplilacja agdy przebiegła OK\n\n"  
                   return Nothing
        Just x -> do
                  liftIO $ putStrLn ("błað kompilatora agdy" ++ x ++ "\n\n\n")
                  liftIO $ putStrLn "usuwam plik agdy po zmianach\n\n"
                  liftIO $ rmAFile env
                  liftIO $ putStrLn "kopiuję z bal plik agdy \n\n"
                  liftIO $ cpAFile env
                  return (Just x)



-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

createConvPart :: String -> (String, String) -> String -> Maybe String -> [Message] ->ConvPart
createConvPart gptIn gptOut afile acres fp =
  ConvPart{ gpt_input =gptIn
          , gpt_res =  fst gptOut
          , pure_code_res = snd gptOut
          , current_agad_file =  afile
          , agda_res = acres
          , promptL = fp
          } 

fConvInput :: AGEnv -> IO String
fConvInput env = do
    templ <- readFile $ fGptTemp env
    agda <- readFile $ (agdaFilesDir env) ++ (agdaFileName env) 
    let x1 = replaceText templ "{function_type}" (taskDescription env)
    let x2 = replaceText x1 "{agda_code}" agda
    return x2

rConvInput :: AGEnv -> String -> IO String
rConvInput env err = do
    templ <- readFile $ fGptTemp env
    agda <- readFile $ (agdaFilesDir env) ++ (agdaFileName env) 
    let x1 = replaceText templ "{agda_code_with_changes}" (agdaFileName env)
    let x2 = replaceText x1 "{compiler_errors}"  err
    return x2
  
  
replaceText :: String -> String -> String -> String
replaceText [] _ _ = []
replaceText str search replace
  | L.take (L.length search) str == search = replace ++ replaceText (L.drop (L.length search) str) search replace
  | otherwise = L.head str : replaceText (L.tail str) search replace

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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



-- data AGEnv = AGEnv
--     { apiKey :: String
--     , agdaFileName :: FilePath
--     , agdaFilesDir :: FilePath
--     , agdaCompilerPath :: FilePath
--     , taskDescription :: String
--     , dbCredentials :: String
--     , operationMode :: OperationMode
--     , maxTurns :: Int
--     , fGptTemp :: FilePath
--     , rGptTemp :: FilePath
--     } deriving (Show)



test2 :: String 
test2 = "\n\nThe implementation of the `add` function can be done recursively by pattern matching on the first argument `n`.\n\nHere's the complete Agda code implementing this function:\n\n```\ndata Nat : Set where\n  zero : Nat\n  suc : Nat → Nat\n\nadd : Nat → Nat → Nat\nadd zero m = m\nadd (suc n) m = suc (add n m)\n```\n\nThe first equation of the `add` function defines the base case for the recursion, where adding zero to any number `m` has no effect and returns `m`.\n\nThe second equation handles the recursive case where we have a successor `suc n` of some number `n`. To add this number to another number `m`, we simply recursively add `n` to `m`, and then add one more to the result with a `suc` constructor."

  
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
