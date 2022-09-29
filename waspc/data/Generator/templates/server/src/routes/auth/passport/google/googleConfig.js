{{={= =}=}}

{=# doesConfigFnExist =}
{=& configFnImportStatement =}
export { {= configFnIdentifier =} as configFn }
{=/ doesConfigFnExist =}
{=^ doesConfigFnExist =}
export { configFn } from './googleDefaults.js'
{=/ doesConfigFnExist =}

{=# doesOnSignInFnExist =}
{=& getUserFieldsFnImportStatement =}
export { {= getUserFieldsFnIdentifier =} as getUserFieldsFn }
{=/ doesOnSignInFnExist =}
{=^ doesOnSignInFnExist =}
export { getUserFieldsFn } from './googleDefaults.js'
{=/ doesOnSignInFnExist =}
