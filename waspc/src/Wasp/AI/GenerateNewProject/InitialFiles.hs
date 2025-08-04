module Wasp.AI.GenerateNewProject.InitialFiles
  ( genAndWriteInitialFiles,
    InitialFilesGenResult (..),
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

data InitialFilesGenResult = InitialFilesGenResult
  { _waspFilePath :: FilePath,
    _prismaFilePath :: FilePath,
    _planRules :: [PlanRule]
  }

genAndWriteInitialFiles ::
  NewProjectDetails ->
  [(Path System (Rel WaspProjectDir) File', Text)] ->
  CodeAgent InitialFilesGenResult
genAndWriteInitialFiles newProjectDetails waspProjectSkeletonFiles = do
  let skeletonFiles = first SP.fromRelFile <$> waspProjectSkeletonFiles
  mapM_ writeNewFile skeletonFiles

  let (waspFile@(waspFilePath, _), waspFilePlanRules) = generateBaseWaspFile newProjectDetails
  writeNewFile waspFile

  writeNewFile $ generatePackageJson newProjectDetails

  let (prismaFile@(prismaFilePath, _), prismaFilePlanRules) = generateBasePrismaFile newProjectDetails

  writeNewFile prismaFile

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

  return $ InitialFilesGenResult waspFilePath prismaFilePath (waspFilePlanRules ++ prismaFilePlanRules)

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
          head: [
            "<link rel='icon' href='/favicon.ico' />",
          ],
          client: {
            rootComponent: import { Layout } from "@src/Layout.jsx",
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

generateBasePrismaFile :: NewProjectDetails -> (File, [PlanRule])
generateBasePrismaFile newProjectDetails = (("schema.prisma", content), planRules)
  where
    content =
      [trimming|
      datasource db {
        provider = "sqlite"
        // Wasp requires that the url is set to the DATABASE_URL environment variable.
        url      = env("DATABASE_URL")
      }

      // Wasp requires the `prisma-client-js` generator to be present.
      generator client {
        provider = "prisma-client-js"
      }
    |]
    planRules = case getProjectAuth newProjectDetails of
      UsernameAndPassword ->
        [ T.unpack
            [trimming|
                Prisma schema MUST have a 'User' model, with following field(s) required:
                  - `id Int @id @default(autoincrement())`
                It is also likely to have a field that refers to some other model that user owns, e.g. `tasks Task[]`.
                |]
        ]

-- TODO: We have duplication here, since package.json is already defined
--   in `basic` templates file. We should find a way to reuse that, so we don't
--   have to keep stuff in sync here.
generatePackageJson :: NewProjectDetails -> File
generatePackageJson newProjectDetails =
  ( "package.json",
    [trimming|
      {
        "name": "${appName}",
        "type": "module",
        "dependencies": {
          "wasp": "file:.wasp/out/sdk/wasp",
          "react": "^18.2.0",
          "react-dom": "^18.2.0",
          "react-router-dom": "^6.26.2",
          "tailwindcss": "^3.2.7"
        },
        "devDependencies": {
          "typescript": "5.8.2",
          "vite": "^7.0.6",
          "@types/react": "^18.0.37",
          "prisma": "5.19.1"
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
      import { Link } from "wasp/client/router";
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
      import { Link } from "wasp/client/router";
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
      import { Link } from "wasp/client/router";
      import { useAuth, logout } from "wasp/client/auth";
      import { Outlet } from "react-router-dom";
      import "./Main.css";

      export const Layout = () => {
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
              <Outlet />
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
  ( "tailwind.config.js",
    [trimming|
      import colors from "tailwindcss/colors";
      import { resolveProjectPath } from "wasp/dev";

      /** @type {import('tailwindcss').Config} */
      export default {
        content: [
          resolveProjectPath('./src/**/*.{js,jsx,ts,tsx}'),
        ],
        theme: {
          extend: {
            colors: {
              primary: {
                ...${primaryColorObject}
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
  ( "postcss.config.js",
    [trimming|
      export default {
        plugins: {
          tailwindcss: {},
          autoprefixer: {},
        },
      }
    |]
  )
