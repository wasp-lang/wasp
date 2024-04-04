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
import Wasp.AI.CodeAgent (writeNewFile)
import Wasp.AI.GenerateNewProject.Common
  ( AuthProvider (..),
    CodeAgent,
    File,
    NewProjectDetails (..),
    getProjectAuth,
    getProjectPrimaryColor,
  )
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

  writeNewFile $ generatePackageJson newProjectDetails

  case getProjectAuth newProjectDetails of
    UsernameAndPassword -> do
      writeNewFile generateLoginJsPage
      writeNewFile generateSignupJsPage

  writeNewFile generateDotEnvServerFile

  -- Tailwind setup: config files + layout component + CSS imports.
  writeNewFile $ generateTailwindConfigFile newProjectDetails
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
    (appAuth, authPlanRules) = case getProjectAuth newProjectDetails of
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
              App MUST have a 'User' entity, with following field(s) required:
                - `id Int @id @default(autoincrement())`
              It is also likely to have a field that refers to some other entity that user owns, e.g. `tasks Task[]`.
              |],
            "One of the pages in the app must have a route path \"/\"."
          ]
        )
    planRules = authPlanRules <> ["Don't generate the Login or Signup pages and routes under any circumstances. They are already generated."]
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
            rootComponent: import { Layout } from "@src/Layout.jsx",
          },
          db: {
            prisma: {
              clientPreviewFeatures: ["extendedWhereUnique"]
            }
          },
          ${appAuth}
        }

        route LoginRoute { path: "/login", to: LoginPage }
        page LoginPage {
          component: import Login from "@src/pages/auth/Login.jsx"
        }
        route SignupRoute { path: "/signup", to: SignupPage }
        page SignupPage {
          component: import Signup from "@src/pages/auth/Signup.jsx"
        }
      |]

-- TODO: We have duplication here, since package.json is already defined
--   in `basic` templates file. We should find a way to reuse that, so we don't
--   have to keep stuff in sync here.
generatePackageJson :: NewProjectDetails -> File
generatePackageJson newProjectDetails =
  ( "package.json",
    [trimming|
      {
        "name": "${appName}",
        "dependencies": {
          "wasp": "file:.wasp/out/sdk/wasp",
          "react": "^18.2.0"
        },
        "devDependencies": {
          "typescript": "^5.1.0",
          "vite": "^4.3.9",
          "@types/react": "^18.0.37",
          "prisma": "4.16.2"
        }
      }

    |]
  )
  where
    appName = T.pack $ _projectAppName newProjectDetails

generateLoginJsPage :: File
generateLoginJsPage =
  ( "src/pages/auth/Login.jsx",
    [trimming|
      import React from "react";
      import { Link } from "react-router-dom";
      import { LoginForm } from "wasp/client/auth";

      export default function Login() {
        return (
          <div className="w-full h-full bg-white">
            <div className="min-w-full min-h-[75vh] flex items-center justify-center">
              <div className="w-full h-full max-w-sm p-5 bg-white">
                <div>
                  <LoginForm
                    appearance={{
                      colors: {
                        brand: 'var(--auth-form-brand)',
                        brandAccent: 'var(--auth-form-brand-accent)',
                        submitButtonText: 'var(--auth-form-submit-button-text-color)',
                      }
                    }}
                  />
                  <div className="mt-4 text-center">
                    If you don't have an account go to{" "}
                    <Link to="/signup" className="text-primary-500 hover:text-primary-800 underline">
                      sign up
                    </Link>
                    .
                  </div>
                </div>
              </div>
            </div>
          </div>
        );
      }
    |]
  )

generateSignupJsPage :: File
generateSignupJsPage =
  ( "src/pages/auth/Signup.jsx",
    [trimming|
      import React from "react";
      import { Link } from "react-router-dom";
      import { SignupForm } from "wasp/client/auth";

      export default function Signup() {
        return (
          <div className="w-full h-full bg-white">
            <div className="min-w-full min-h-[75vh] flex items-center justify-center">
              <div className="w-full h-full max-w-sm p-5 bg-white">
                <div>
                  <SignupForm
                    appearance={{
                      colors: {
                        brand: 'var(--auth-form-brand)',
                        brandAccent: 'var(--auth-form-brand-accent)',
                        submitButtonText: 'var(--auth-form-submit-button-text-color)',
                      }
                    }}
                  />
                  <div className="mt-4 text-center">
                    If you already have an account go to{" "}
                    <Link to="/login" className="text-primary-500 hover:text-primary-800 underline">
                      login
                    </Link>
                    .
                  </div>
                </div>
              </div>
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
  ( "src/Main.css",
    [trimming|
      @tailwind base;
      @tailwind components;
      @tailwind utilities;

      :root {
        --auth-form-brand: theme(colors.primary.500);
        --auth-form-brand-accent: theme(colors.primary.600);
        --auth-form-submit-button-text-color: theme(colors.white);
      }
    |]
  )

generateLayoutComponent :: NewProjectDetails -> File
generateLayoutComponent newProjectDetails =
  ( "src/Layout.jsx",
    [trimming|
      import { Link } from "react-router-dom";
      import { useAuth, logout } from "wasp/client/auth";
      import "./Main.css";

      export const Layout = ({ children }) => {
        const { data: user } = useAuth();

        return (
          <div className="flex flex-col min-h-screen">
            <header className="bg-primary-800 text-white p-4">
              <div className="container mx-auto px-4 py-2 flex justify-between">
                <Link to="/">
                  <h1 className="text-xl2 font-semibold">${appName}</h1>
                </Link>
                { user ? (
                  <span>
                    Hi, {user.identities.username?.id}!{' '}
                    <button onClick={logout} className="text-xl2 underline">
                      (Log out)
                    </button>
                  </span>
                ) : (
                  <Link to="/login">
                    <h1 className="text-xl2 underline">Log in</h1>
                  </Link>
                )}
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

generateTailwindConfigFile :: NewProjectDetails -> File
generateTailwindConfigFile newProjectDetails =
  ( "tailwind.config.cjs",
    [trimming|
      const { resolveProjectPath } = require('wasp/dev')
      const colors = require('tailwindcss/colors')

      /** @type {import('tailwindcss').Config} */
      module.exports = {
        content: [
          resolveProjectPath('./src/**/*.{js,jsx,ts,tsx}'),
        ],
        theme: {
          extend: {
            colors: {
              primary: {
                50:  ${primaryColorObject}[50],
                100: ${primaryColorObject}[100],
                200: ${primaryColorObject}[200],
                300: ${primaryColorObject}[300],
                400: ${primaryColorObject}[400],
                500: ${primaryColorObject}[500],
                600: ${primaryColorObject}[600],
                700: ${primaryColorObject}[700],
                800: ${primaryColorObject}[800],
                900: ${primaryColorObject}[900],
              }
            }
          },
        },
      }
    |]
  )
  where
    primaryColorName = getProjectPrimaryColor newProjectDetails
    primaryColorObject = T.pack $ "colors." <> primaryColorName

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
