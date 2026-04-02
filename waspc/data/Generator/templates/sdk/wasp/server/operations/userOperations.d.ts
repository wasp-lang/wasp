{{={= =}=}}
{=# operations =}
declare module '{= virtualModulePath =}' {
  import type { FromOperationsRegistry } from 'wasp/types'
  {=# isQuery =}
  import type { {= typeName =} } from './queries/types.js'
  {=/ isQuery =}
  {=^ isQuery =}
  import type { {= typeName =} } from './actions/types.js'
  {=/ isQuery =}

  type UserOperation = FromOperationsRegistry<'{= operationName =}', {= typeName =}>;
  export const {= exportName =}: UserOperation;
}
{=/ operations =}
