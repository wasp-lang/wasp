module Wasp.Cli.Command.UseRecipe.Auth.Local where

import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.UseRecipe.Auth.Common (copyLoginPage, copySignupPage, showRouteAndPageDSL)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Terminal as Term

useLocal :: Command ()
useLocal = do
  copyLoginPage
  copySignupPage

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following entity definition to your main.wasp file:\n"
  cliSendMessageC $
    Msg.Info $
      unlines
        [ "entity User {=psl",
          "  id                        Int           @id @default(autoincrement())",
          "  username                  String        @unique",
          "  password                  String",
          "psl=}"
        ]

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following auth block to the app block in your main.wasp file:\n"
  cliSendMessageC $
    Msg.Info $
      unlines
        [ "auth: {",
          "  userEntity: User,",
          "  methods: {",
          "    usernameAndPassword: {},",
          "  },",
          "  onAuthFailedRedirectTo: \"/login\"",
          "}"
        ]

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following routes and pages to your main.wasp file:\n"
  cliSendMessageC $ Msg.Info $ showRouteAndPageDSL "LoginRoute" "/login" "LoginPage"
  cliSendMessageC $ Msg.Info $ showRouteAndPageDSL "SignupRoute" "/signup" "SignupPage"
