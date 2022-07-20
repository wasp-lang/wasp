{{={= =}=}}

{=# doesConfigFnExist =}
{=& configFnImportStatement =}
export { {= configFnIdentifier =} as configFn }
{=/ doesConfigFnExist =}
{=^ doesConfigFnExist =}
export { configFn } from './googleDefaults.js'
{=/ doesConfigFnExist =}

{=# doesOnSignInFnExist =}
{=& onSignInFnImportStatement =}
export { {= onSignInFnIdentifier =} as onSignInFn }
{=/ doesOnSignInFnExist =}
{=^ doesOnSignInFnExist =}
export { onSignInFn } from './googleDefaults.js'
{=/ doesOnSignInFnExist =}
