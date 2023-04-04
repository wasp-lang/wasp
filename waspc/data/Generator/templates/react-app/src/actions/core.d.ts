import { Action } from '.'

export function createAction<BackendAction extends GenericBackendAction>(
    actionRoute: string,
    entitiesUsed: unknown[]
): ActionFor<BackendAction>

type ActionFor<BackendAction extends GenericBackendAction> =
  Action<Parameters<BackendAction>[0], Awaited<ReturnType<BackendAction>>>

type GenericBackendAction = (args: never, context: any) => Promise<unknown>
