import { type Action } from '../core.js'
import type { Expand, _Awaited, _ReturnType } from 'wasp/universal/types'

// PRIVATE API
export function createAction<BackendAction extends GenericBackendAction>(
  actionRoute: string,
  entitiesUsed: unknown[]
): ActionFor<BackendAction>

type ActionFor<BackendAction extends GenericBackendAction> = Expand<
  Action<Parameters<BackendAction>[0], _Awaited<_ReturnType<BackendAction>>>
>

type GenericBackendAction = (args: never, context: any) => unknown
