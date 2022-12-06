{{={= =}=}}

{=# doesConfigFnExist =}
{=& configFnImportStatement =}
export { {= configFnIdentifier =} as configFn }
{=/ doesConfigFnExist =}
{=^ doesConfigFnExist =}
export { configFn } from './defaults.js'
{=/ doesConfigFnExist =}

{=# doesGetUserFieldsFnExist =}
{=& getUserFieldsFnImportStatement =}
export { {= getUserFieldsFnIdentifier =} as getUserFieldsFn }
{=/ doesGetUserFieldsFnExist =}
{=^ doesGetUserFieldsFnExist =}
export { getUserFieldsFn } from './defaults.js'
{=/ doesGetUserFieldsFnExist =}
