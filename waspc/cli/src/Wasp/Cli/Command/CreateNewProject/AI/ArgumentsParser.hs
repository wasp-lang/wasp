module Wasp.Cli.Command.CreateNewProject.AI.ArgumentsParser where

import Data.List (intersperse)
import qualified Options.Applicative as Opt
import Options.Applicative.Help (Doc, indent, line)

data NewProjectAIArgs = NewProjectAIArgs
  { projectDestination :: NewProjectAIDestination,
    projectName :: String,
    appDescription :: String,
    projectConfigJson :: String
  }
  deriving (Show)

data NewProjectAIDestination
  = ToDisk
  | ToStdout
  deriving (Show)

newProjectAiParserInfo :: Opt.ParserInfo NewProjectAIArgs
newProjectAiParserInfo =
  Opt.info
    newProjectAiParser
    ( Opt.fullDesc
        <> Opt.progDescDoc (Just description)
        <> Opt.footerDoc (Just footer)
    )
  where
    description =
      toLines
        [ "Uses AI to create a new Wasp project just based on the app name and the description.",
          "You can do the same thing with `wasp new` interactively."
        ]

    footer =
      toLines
        [ "Examples:",
          indent 2 $
            toLines
              [ "wasp new:ai ButtonApp \"One page with button\" '{}'",
                "wasp new:ai ButtonApp \"One page with button\" '{ \"defaultGptTemperature\": 0.5, \"codingGptModel\": \"gpt-4-1106-preview\" }\'"
              ]
        ]

newProjectAiParser :: Opt.Parser NewProjectAIArgs
newProjectAiParser =
  NewProjectAIArgs
    <$> projectDestinationParser
    <*> projectNameParser
    <*> appDescriptionParser
    <*> projectConfigJsonParser
  where
    projectDestinationParser =
      Opt.flag
        ToDisk
        ToStdout
        ( Opt.long "stdout"
            <> Opt.help "Output the generated project files to stdout instead of creating them on disk."
        )

    projectNameParser =
      Opt.strArgument $
        Opt.metavar "PROJECT_NAME"

    appDescriptionParser =
      Opt.strArgument $
        Opt.metavar "APP_DESCRIPTION"

    projectConfigJsonParser =
      Opt.strArgument $
        Opt.metavar "PROJECT_CONFIG_JSON"
          <> Opt.helpDoc
            ( Just $
                toLines
                  [ "A JSON string representing additional configuration for the Wasp project.",
                    "The following fields are supported:",
                    indent 2 $
                      toLines
                        [ "{",
                          indent 2 $
                            toLines
                              [ "\"defaultGptTemperature\"?: number (from 0 to 2)",
                                "\"planningGptModel\"?: string (OpenAI model name)",
                                "\"codingGptModel\"?: string (OpenAI model name)",
                                "\"primaryColor\"?: string (Tailwind color name)"
                              ],
                          "}"
                        ]
                  ]
            )
          <> Opt.value "{}"

toLines :: [Doc] -> Doc
toLines = mconcat . intersperse line
