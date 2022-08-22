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
export { {= onSignInFnIdentifier =} as firstSignInConfig }
{=/ doesOnSignInFnExist =}
{=^ doesOnSignInFnExist =}
export { firstSignInConfig } from './googleDefaults.js'
{=/ doesOnSignInFnExist =}
