// @ts-nocheck
import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";

import { createAuthRequiredPage } from "wasp/client/app"

// These files are used from user-land and the import paths below are relative to the
// user's project dir, and not the SDK:
import { HomePage } from './src/pages/HomePage'
import { CatchAllPage } from './src/pages/CatchAllPage'
import SignupPage from './src/features/auth/pages/Signup'
import LoginPage from './src/features/auth/pages/Login'
import { PasswordReset as PasswordResetPage } from './src/features/auth/pages/PasswordReset'
import { EmailVerification as EmailVerificationPage } from './src/features/auth/pages/EmailVerification'
import { RequestPasswordReset as RequestPasswordResetPage } from './src/features/auth/pages/RequestPasswordReset'
import { ProfilePage } from './src/features/auth/pages/ProfilePage'
import { ManualSignupPage } from './src/features/auth/pages/ManualSignupPage'
import { CustomSignupPage } from './src/features/auth/pages/CustomSignupPage'
import { TasksPage } from './src/features/operations/pages/TasksPage'
import { TaskDetailPage as TaskPage } from './src/features/operations/pages/TaskDetailPage'
import { SerializationPage } from './src/features/operations/pages/SerializationPage'
import { JobsPage } from './src/features/jobs/pages/JobsPage'
import { ApisPage } from './src/features/apis/pages/ApisPage'
import { ListPage as CrudList } from './src/features/crud/pages/ListPage'
import { DetailPage as CrudDetail } from './src/features/crud/pages/DetailPage'
import { StreamingTestPage as StreamingPage } from './src/features/streaming/pages/StreamingTestPage'
import { ChatPage } from './src/features/chat/pages/ChatPage'

const routesMapping = {
  HomeRoute: HomePage,
  CatchAllRoute: CatchAllPage,
  SignupRoute: SignupPage,
  LoginRoute: LoginPage,
  PasswordResetRoute: PasswordResetPage,
  EmailVerificationRoute: EmailVerificationPage,
  RequestPasswordResetRoute: RequestPasswordResetPage,
  ProfileRoute: createAuthRequiredPage(ProfilePage),
  ManualSignupRoute: ManualSignupPage,
  CustomSignupRoute: CustomSignupPage,
  TasksRoute: createAuthRequiredPage(TasksPage),
  TaskRoute: createAuthRequiredPage(TaskPage),
  SerializationRoute: SerializationPage,
  JobsRoute: createAuthRequiredPage(JobsPage),
  ApisRoute: ApisPage,
  CrudListRoute: createAuthRequiredPage(CrudList),
  CrudDetailRoute: createAuthRequiredPage(CrudDetail),
  StreamingRoute: StreamingPage,
  ChatRoute: createAuthRequiredPage(ChatPage),
} as const;

import { App as App_ext } from './src/App'

import { setup as setup_ext } from './src/clientSetup'

await setup_ext()

initializeQueryClient()

const rootElement =
  <App_ext />

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
