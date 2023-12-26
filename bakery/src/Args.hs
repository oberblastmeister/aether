module Args
  ( parse,
    Command (..),
    Options (..),
  )
where

import Options.Applicative

data Options = Options
  { directory :: Maybe FilePath,
    command :: Command
  }

parse :: IO Options
parse = customExecParser p opts
  where
    opts = info (helper <*> pOptions) idm
    p = prefs showHelpOnEmpty

pOptions :: Parser Options
pOptions =
  Options
    <$> optional
      ( strOption
          ( long "directory"
              <> metavar "DIRECTORY"
              <> help "Specify the directory"
          )
      )
    <*> pCommand

pCommand :: Parser Command
pCommand =
  subparser $
    mconcat
      [ command "build" $ info pBuild $ progDesc "Build",
        command "test" $ info pTest $ progDesc "Test",
        command "run" $ info pRun $ progDesc "Run",
        command "debug" $ info pDebug $ progDesc "Debug",
        command "clean" $ info (pure Clean) $ progDesc "Clean the build directory"
      ]

pRun :: Parser Command
pRun = pure Run

pDebug :: Parser Command
pDebug = pure Debug

pBuild :: Parser Command
pBuild = pure Build

pTest :: Parser Command
pTest = Test <$> many (strArgument (metavar "TEST_ARG"))

data Command
  = Build
  | Test [String]
  | Run
  | Debug
  | Clean
