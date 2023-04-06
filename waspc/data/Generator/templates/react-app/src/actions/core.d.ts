import { type Action } from '.'
import type { Expand, _Awaited } from '../universal/types'

export function createAction<BackendAction extends GenericBackendAction>(
  actionRoute: string,
  entitiesUsed: unknown[]
): ActionFor<BackendAction>

type ActionFor<BackendAction extends GenericBackendAction> = Expand<
  Action<Parameters<BackendAction>[0], _Awaited<ReturnType<BackendAction>>>
>

type GenericBackendAction = (args: never, context: any) => Promise<unknown>
