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
export { {= onSignInFnIdentifier =} as getUserFields }
{=/ doesOnSignInFnExist =}
{=^ doesOnSignInFnExist =}
export { getUserFields } from './googleDefaults.js'
{=/ doesOnSignInFnExist =}
