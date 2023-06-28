module Wasp.AI.GenerateNewProject.Skeleton
  ( generateAndWriteProjectSkeletonAndPresetFiles,
  )
where

import Control.Arrow (first)
import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import StrongPath (File', Path, Rel)
import qualified StrongPath as SP
import StrongPath.Types (System)
import Wasp.AI.CodeAgent (CodeAgent, writeNewFile)
import Wasp.AI.GenerateNewProject.Common (AuthProvider (..), File, NewProjectDetails (..))
import Wasp.AI.GenerateNewProject.Plan (PlanRule)
import Wasp.Project (WaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Version

generateAndWriteProjectSkeletonAndPresetFiles ::
  NewProjectDetails ->
  [(Path System (Rel WaspProjectDir) File', Text)] ->
  CodeAgent (FilePath, [PlanRule])
generateAndWriteProjectSkeletonAndPresetFiles newProjectDetails waspProjectSkeletonFiles = do
  let skeletonFiles = first SP.fromRelFile <$> waspProjectSkeletonFiles
  mapM_ writeNewFile skeletonFiles

  let (waspFile@(waspFilePath, _), planRules) = generateBaseWaspFile newProjectDetails
  writeNewFile waspFile

  case _projectAuth newProjectDetails of
    UsernameAndPassword -> do
      writeNewFile generateLoginJsPage
      writeNewFile generateSignupJsPage

  writeNewFile generateDotEnvServerFile

  -- Tailwind setup: config files + layout component + CSS imports.
  writeNewFile generateTailwindConfigFile
  writeNewFile generatePostcssConfigFile
  writeNewFile $ generateLayoutComponent newProjectDetails
  writeNewFile generateMainCssFile

  return (waspFilePath, planRules)

generateBaseWaspFile :: NewProjectDetails -> (File, [PlanRule])
generateBaseWaspFile newProjectDetails = ((path, content), planRules)
  where
    path = "main.wasp"
    appName = T.pack $ _projectAppName newProjectDetails
    appTitle = appName
    waspVersionRange = T.pack . show $ SV.backwardsCompatibleWith Wasp.Version.waspVersion
    (appAuth, authPlanRules) = case _projectAuth newProjectDetails of
      UsernameAndPassword ->
        ( [trimming|
            auth: {
              userEntity: User,
              methods: {
                usernameAndPassword: {}
              },
              onAuthFailedRedirectTo: "/login",
              onAuthSucceededRedirectTo: "/"
            },
        |],
          [ "App uses username and password authentication.",
            T.unpack
              [trimming|
              App MUST have a 'User' entity, with following fields required:
                - `id Int @id @default(autoincrement())`
                - `username String @unique`
                - `password String`
              |],
            "One of the pages in the app must have a route path \"/\"."
          ]
        )
    planRules = authPlanRules <> ["Don't generate the Login or Signup page."]
    -- TODO: add some commented out lines to wasp file that showcase other features? jobs, api,
    --   serverSetup, sockets, ... .
    content =
      [trimming|
        app ${appName} {
          wasp: {
            version: "${waspVersionRange}"
          },
          title: "${appTitle}",
          client: {
            rootComponent: import { Layout } from "@client/Layout.jsx",
          },
          ${appAuth}
        }

        route LoginRoute { path: "/login", to: LoginPage }
        page LoginPage {
          component: import Login from "@client/pages/Login.jsx"
        }
        route SignupRoute { path: "/signup", to: SignupPage }
        page SignupPage {
          component: import Signup from "@client/pages/Signup.jsx"
        }
      |]

generateLoginJsPage :: File
generateLoginJsPage =
  ( "src/client/pages/Login.jsx",
    [trimming|
      import React from "react";
      import { Link } from "react-router-dom";
      import { LoginForm } from "@wasp/auth/forms/Login";

      export default function Login() {
        return (
          <div>
            <LoginForm />
            <div className="mt-4 text-center">
              If you don't have an account go to{" "}
              <Link to="/signup" className="text-slate-500 hover:text-slate-800">
                sign up
              </Link>
            </div>
          </div>
        );
      }
    |]
  )

generateSignupJsPage :: File
generateSignupJsPage =
  ( "src/client/pages/Signup.jsx",
    [trimming|
      import React from "react";
      import { Link } from "react-router-dom";
      import { SignupForm } from "@wasp/auth/forms/Signup";

      export default function Signup() {
        return (
          <div>
            <SignupForm />
            <div className="mt-4 text-center">
              If you already have an account go to{" "}
              <Link to="/login" className="text-slate-500 hover:text-slate-800">
                login
              </Link>
            </div>
          </div>
        );
      }
    |]
  )

generateDotEnvServerFile :: File
generateDotEnvServerFile =
  ( ".env.server",
    [trimming|
      # Here you can define env vars to pass to the server.
      # MY_ENV_VAR=foobar
    |]
  )

generateMainCssFile :: File
generateMainCssFile =
  ( "src/client/Main.css",
    [trimming|
      @tailwind base;
      @tailwind components;
      @tailwind utilities;
    |]
  )

generateLayoutComponent :: NewProjectDetails -> File
generateLayoutComponent newProjectDetails =
  ( "src/client/Layout.jsx",
    [trimming|
      import "./Main.css";

      export const Layout = ({ children }) => {
        return (
          <div className="flex flex-col min-h-screen">
            <header className="bg-slate-800 text-white p-4">
              <div className="container mx-auto px-4 py-2">
                <h1 className="text-xl2 font-semibold">${appName}</h1>
              </div>
            </header>
            <main className="container mx-auto px-4 py-2 flex-grow">
              {children}
            </main>
            <footer>
              <div className="container mx-auto p-4">
                <p className="text-center text-gray-500 text-sm">
                  ${appName} ~ Powered by Wasp
                </p>
              </div>
            </footer>
          </div>
        );
      };
    |]
  )
  where
    appName = T.pack $ _projectAppName newProjectDetails

generateTailwindConfigFile :: File
generateTailwindConfigFile =
  ( "tailwind.config.cjs",
    [trimming|
      /** @type {import('tailwindcss').Config} */
      module.exports = {
        content: [
          "./src/**/*.{js,jsx,ts,tsx}",
        ],
        theme: {
          extend: {},
        },
      }
    |]
  )

generatePostcssConfigFile :: File
generatePostcssConfigFile =
  ( "postcss.config.cjs",
    [trimming|
      module.exports = {
        plugins: {
          tailwindcss: {},
          autoprefixer: {},
        },
      }
    |]
  )
