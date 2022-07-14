{{={= =}=}}

{=# doesConfigFnExist =}
export const userConfigFnExists = true
{=& configFnImportStatement =}
export { {= configFnIdentifier =} as configFn }
{=/ doesConfigFnExist =}
{=^ doesConfigFnExist =}
export const userConfigFnExists = false
export { configFn } from './googleDefaults.js'
{=/ doesConfigFnExist =}

{=# doesOnSignInFnExist =}
{=& onSignInFnImportStatement =}
export { {= onSignInFnIdentifier =} as onSignInFn }
{=/ doesOnSignInFnExist =}
{=^ doesOnSignInFnExist =}
export { onSignInFn } from './googleDefaults.js'
{=/ doesOnSignInFnExist =}
