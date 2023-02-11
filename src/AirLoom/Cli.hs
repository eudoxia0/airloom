{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module AirLoom.Cli (entrypoint) where

import Data.Typeable (Typeable)
import Options.Applicative
import Data.Aeson (encode)
import qualified AirLoom.Store as Store
import AirLoom.Parser (parseSourceFile)
import AirLoom.Lift (liftFragments)
import Data.Either (partitionEithers)
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad (foldM)

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
execWeave files frag output = putStrLn $ "weave " ++ show files ++ " with " ++ frag ++ " to " ++ output
