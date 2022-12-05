{{={= =}=}}

{=# doesConfigFnExist =}
{=& configFnImportStatement =}
export { {= configFnIdentifier =} as configFn }
{=/ doesConfigFnExist =}
{=^ doesConfigFnExist =}
export { configFn } from './githubDefaults.js'
{=/ doesConfigFnExist =}

{=# doesOnSignInFnExist =}
{=& getUserFieldsFnImportStatement =}
export { {= getUserFieldsFnIdentifier =} as getUserFieldsFn }
{=/ doesOnSignInFnExist =}
{=^ doesOnSignInFnExist =}
export { getUserFieldsFn } from './githubDefaults.js'
{=/ doesOnSignInFnExist =}
