{{={= =}=}}
// PRIVATE API (web-app)
export { WaspApp } from './components/WaspApp'

// PRIVATE API (web-app)
export {
  reportClientError,
  listenForUncaughtClientErrors,
  registerClientOnErrorFn,
} from '@wasp/lib-sdk-core/browser'

{=# isAuthEnabled =}
// PRIVATE API (web-app)
export { createAuthRequiredPage } from './pages/createAuthRequiredPage'
{=/ isAuthEnabled =}
