{{={= =}=}}

{=# doesConfigFnExist =}
{=& configFnImportStatement =}
export { {= configFnIdentifier =} as configFn }
{=/ doesConfigFnExist =}
{=^ doesConfigFnExist =}
export { configFn } from './githubDefaults.js'
{=/ doesConfigFnExist =}

{=# doesGetUserFieldsFnExist =}
{=& getUserFieldsFnImportStatement =}
export { {= getUserFieldsFnIdentifier =} as getUserFieldsFn }
{=/ doesGetUserFieldsFnExist =}
{=^ doesGetUserFieldsFnExist =}
export { getUserFieldsFn } from './githubDefaults.js'
{=/ doesGetUserFieldsFnExist =}
