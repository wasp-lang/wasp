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
  export const {= exportName =}: FromOperationsRegistry<'{= operationName =}', {= typeName =}>
}
{=/ operations =}
