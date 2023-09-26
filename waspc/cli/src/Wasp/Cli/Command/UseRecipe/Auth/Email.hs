module Wasp.Cli.Command.UseRecipe.Auth.Email where

import Wasp.Cli.Command
import Wasp.Cli.Command.Message
import Wasp.Cli.Command.UseRecipe.Auth.Common (copyEmailVerificationPage, copyLoginPage, copyRequestPasswordResetPage, copyResetPasswordPage, copySignupPage, showRouteAndPageDSL)
import Wasp.Cli.Command.UseRecipe.EmailSender (useEmailSender)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Terminal as Term

useEmail :: Command ()
useEmail = do
  useEmailSender

  copyLoginPage
  copySignupPage
  copyRequestPasswordResetPage
  copyResetPasswordPage
  copyEmailVerificationPage

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following entity definition to your main.wasp file:\n"
  cliSendMessageC $
    Msg.Info $
      unlines
        [ "entity User {=psl",
          "  id                        Int           @id @default(autoincrement())",
          "  email                     String?       @unique",
          "  password                  String?",
          "  isEmailVerified           Boolean       @default(false)",
          "  emailVerificationSentAt   DateTime?",
          "  passwordResetSentAt       DateTime?",
          "psl=}"
        ]

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following auth block to the app block in your main.wasp file:\n"
  cliSendMessageC $
    Msg.Info $
      unlines
        [ "auth: {",
          "  userEntity: User,",
          "  methods: {",
          "    email: {",
          "      fromField: {",
          "        name: \"My App\",",
          "        email: \"myapp@domain.com\"",
          "      },",
          "      emailVerification: {",
          "        clientRoute: EmailVerificationRoute,",
          "      },",
          "      passwordReset: {",
          "        clientRoute: ResetPasswordRoute",
          "      },",
          "      allowUnverifiedLogin: false,",
          "    },",
          "  },",
          "  onAuthFailedRedirectTo: \"/login\"",
          "}"
        ]

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following routes and pages to your main.wasp file:\n"
  cliSendMessageC $ Msg.Info $ showRouteAndPageDSL "LoginRoute" "/login" "LoginPage"
  cliSendMessageC $ Msg.Info $ showRouteAndPageDSL "SignupRoute" "/signup" "SignupPage"
  cliSendMessageC $ Msg.Info $ showRouteAndPageDSL "RequestPasswordResetRoute" "/request-password-reset" "RequestPasswordResetPage"
  cliSendMessageC $ Msg.Info $ showRouteAndPageDSL "ResetPasswordRoute" "/reset-password" "ResetPasswordPage"
  cliSendMessageC $ Msg.Info $ showRouteAndPageDSL "EmailVerificationRoute" "/email-verification" "EmailVerificationPage"
