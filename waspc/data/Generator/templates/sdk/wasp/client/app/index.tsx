{{={= =}=}}
{=# isAuthEnabled =}
// PRIVATE API (web-app)
export { createAuthRequiredPage } from './pages/createAuthRequiredPage'
{=/ isAuthEnabled =}

{=! This file might be empty if auth is not enabled, so we export an empty object to force module syntax and avoid bundling errors. =}
export {}
