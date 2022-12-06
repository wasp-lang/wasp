{{={= =}=}}

{=# doesConfigFnExist =}
{=& configFnImportStatement =}
export { {= configFnIdentifier =} as configFn }
{=/ doesConfigFnExist =}
{=^ doesConfigFnExist =}
export { configFn } from './googleDefaults.js'
{=/ doesConfigFnExist =}

{=# doesGetUserFieldsFnExist =}
{=& getUserFieldsFnImportStatement =}
export { {= getUserFieldsFnIdentifier =} as getUserFieldsFn }
{=/ doesGetUserFieldsFnExist =}
{=^ doesGetUserFieldsFnExist =}
export { getUserFieldsFn } from './googleDefaults.js'
{=/ doesGetUserFieldsFnExist =}
