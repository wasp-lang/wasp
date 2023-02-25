{{={= =}=}}

{=# configFn.isDefined =}
{=& configFn.importStatement =}
export { {= configFn.importIdentifier =} as configFn }
{=/ configFn.isDefined =}
{=^ configFn.isDefined =}
export { configFn } from './defaults.js'
{=/ configFn.isDefined =}

{=# userFieldsFn.isDefined =}
{=& userFieldsFn.importStatement =}
export { {= userFieldsFn.importIdentifier =} as getUserFieldsFn }
{=/ userFieldsFn.isDefined =}
{=^ userFieldsFn.isDefined =}
export { getUserFieldsFn } from './defaults.js'
{=/ userFieldsFn.isDefined =}
