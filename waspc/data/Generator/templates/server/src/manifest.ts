{{={= =}=}}
{=# hasOperations =}
import { registerOperation } from 'wasp/server/operations/operationsRegistry'
{=/ hasOperations =}
{=# operations =}
{=& jsFn.importStatement =}
{=/ operations =}
{=# operations =}
registerOperation('{= operationName =}', {= jsFn.importIdentifier =})
{=/ operations =}
