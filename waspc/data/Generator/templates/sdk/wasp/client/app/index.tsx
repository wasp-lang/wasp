{{={= =}=}}
// PRIVATE API (web-app)
export { WaspApp } from './components/WaspApp'

// PRIVATE API (web-app)
export {
  listenForUncaughtClientErrors,
  registerClientOnErrorFn,
  reportClientError,
  reportRenderError,
} from "@wasp/sdk-core/browser";

{=# isAuthEnabled =}
// PRIVATE API (web-app)
export { createAuthRequiredPage } from './pages/createAuthRequiredPage'
{=/ isAuthEnabled =}
