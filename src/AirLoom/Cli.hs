{-# LANGUAGE DeriveDataTypeable #-}

module AirLoom.Cli (entrypoint) where

import Data.Semigroup ((<>))
import Data.Typeable (Typeable)
import Options.Applicative

data Command
  = Lift {liftFiles :: [String], outputFile :: String}
  | Weave {weaveFiles :: [String], fragmentsFile :: String, outputFile :: String}
  deriving (Show, Typeable)

cliParser :: Parser Command
cliParser =
  hsubparser $ liftC <> weaveC
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

entrypoint :: IO ()
entrypoint = do
  cli <- execParser opts
  case cli of
    Lift files output -> putStrLn $ "lift " ++ show files ++ " to " ++ output
    Weave files frag output -> putStrLn $ "weave " ++ show files ++ " with " ++ frag ++ " to " ++ output
  where
    opts =
      info
        (cliParser <**> helper)
        (fullDesc <> progDesc "Air Loom is a language-agnostic reverse literate programming tool." <> header "Air Loom - A reverse literate programming tool")
