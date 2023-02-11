{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module AirLoom.Cli (entrypoint) where

import AirLoom.Lift (liftFragments)
import AirLoom.Parser (parseDocFile, parseSourceFile)
import qualified AirLoom.Store as Store
import AirLoom.Weave (weave)
import Control.Monad (foldM)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (partitionEithers)
import Data.Typeable (Typeable)
import Options.Applicative
import System.Exit

data Command
  = Lift {liftFiles :: [String], outputFile :: String}
  | Weave {weaveFiles :: [String], fragmentsFile :: String, outputFile :: String}
  deriving (Show, Typeable)

-- Commands

cliParser :: Parser Command
cliParser =
  subparser $ liftC <> weaveC
  where
    liftC = command "lift" (info liftCommand (progDesc "Lift fragments out of source code."))
    weaveC = command "weave" (info weaveCommand (progDesc "Weave fragments and documentation together."))

liftCommand :: Parser Command
liftCommand =
  Lift
    <$> some (strArgument (metavar "FILES..."))
    <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Fragments will be written as JSON to this file.")

weaveCommand :: Parser Command
weaveCommand =
  Weave
    <$> some (strArgument (metavar "FILES..."))
    <*> strOption (long "fragments" <> short 'f' <> metavar "FRAGS" <> help "Fragments will be read from this JSON file.")
    <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output will be written to this file.")

synopsis :: String
synopsis =
  "Air Loom - A reverse literate programming tool"

desc :: String
desc =
  "Air Loom is a language-agnostic reverse literate programming tool."

-- Execution

entrypoint :: IO ()
entrypoint = do
  cmd <- customExecParser (prefs showHelpOnEmpty) opts
  execute cmd
  where
    opts =
      info
        (cliParser <**> helper)
        (fullDesc <> progDesc desc <> header synopsis)

execute :: Command -> IO ()
execute cmd =
  case cmd of
    Lift files output -> execLift files output
    Weave files frag output -> execWeave files frag output

execLift :: [String] -> String -> IO ()
execLift files output = do
  contents <- mapM readFile files
  let stores = map (liftFragments . parseSourceFile) contents
      (errors, result) = partitionEithers stores
  case errors of
    [] -> do
      let mergeResult = foldM Store.merge (Store.empty) result
      case mergeResult of
        Left err -> do
          putStrLn $ "Error merging stores: " ++ show err
          exitWith (ExitFailure (-1))
        Right merged -> BSL.writeFile output (encode merged)
    _ -> do
      putStrLn "Lifting failed: "
      mapM_ (putStrLn . show) errors
      exitWith (ExitFailure (-1))

execWeave :: [String] -> String -> String -> IO ()
execWeave files frags output = do
  contents :: [String] <- mapM readFile files
  store <- eitherDecode <$> BSL.readFile frags
  case store of
    Left err -> do
      putStrLn $ "Error reading store: " ++ err
      exitWith (ExitFailure (-1))
    Right store' -> do
      let docLines = unlines contents
          docs = parseDocFile docLines
          result = weave docs store'
      case result of
        Left err -> do
          putStrLn $ "Weaving failed: " ++ show err
          exitWith (ExitFailure (-1))
        Right res -> writeFile output (unlines res)