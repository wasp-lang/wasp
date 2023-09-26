module Wasp.Cli.Command.UseRecipe.Auth.Social where

import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.UseRecipe.Auth.Common (copyLoginPage, showRouteAndPageDSL)
import Wasp.Cli.Command.UseRecipe.Common (appendToServerEnv)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Terminal as Term

useGoogle :: Command ()
useGoogle = do
  copyLoginPage

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following entities to your main.wasp file:\n"
  printSocialLoginEntities

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following auth block to the app block in your main.wasp file:\n"
  printSocialLoginAuthBlock "google"

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following routes and pages to your main.wasp file:\n"
  cliSendMessageC $ Msg.Info $ showRouteAndPageDSL "LoginRoute" "/login" "LoginPage"

  appendToServerEnv $ unlines ["GOOGLE_CLIENT_ID=\"\"", "GOOGLE_CLIENT_SECRET=\"\""]
  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Fill the values for GOOGLE_CLIENT_ID and GOOGLE_CLIENT_SECRET in .env.server file."

useGithub :: Command ()
useGithub = do
  copyLoginPage

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following entities to your main.wasp file:\n"
  printSocialLoginEntities

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following auth block to the app block in your main.wasp file:\n"
  printSocialLoginAuthBlock "github"

  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Add the following routes and pages to your main.wasp file:\n"
  cliSendMessageC $ Msg.Info $ showRouteAndPageDSL "LoginRoute" "/login" "LoginPage"

  appendToServerEnv $ unlines ["GITHUB_CLIENT_ID=\"\"", "GITHUB_CLIENT_SECRET=\"\""]
  cliSendMessageC $ Msg.Info $ Term.applyStyles [Term.Bold] "Fill the values for GITHUB_CLIENT_ID and GITHUB_CLIENT_SECRET in .env.server file."

printSocialLoginEntities :: Command ()
printSocialLoginEntities = do
  cliSendMessageC $
    Msg.Info $
      unlines
        [ "entity User {=psl",
          "  id                        Int           @id @default(autoincrement())",
          "  externalAuthAssociations  SocialLogin[]",
          "psl=}"
        ]
  cliSendMessageC $
    Msg.Info $
      unlines
        [ "entity SocialLogin {=psl",
          "  id          Int       @id @default(autoincrement())",
          "  provider    String",
          "  providerId  String",
          "  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)",
          "  userId      Int",
          "  createdAt   DateTime  @default(now())",
          "  @@unique([provider, providerId, userId])",
          "psl=}"
        ]

printSocialLoginAuthBlock :: String -> Command ()
printSocialLoginAuthBlock provider = do
  cliSendMessageC $
    Msg.Info $
      unlines
        [ "auth: {",
          "  userEntity: User,",
          "  externalAuthEntity: SocialLogin,",
          "  methods: {",
          "    " <> provider <> ": {},",
          "  },",
          "  onAuthFailedRedirectTo: \"/login\"",
          "}"
        ]
