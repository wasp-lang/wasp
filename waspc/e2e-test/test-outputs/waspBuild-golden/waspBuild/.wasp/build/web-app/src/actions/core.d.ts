import { Action } from '.'

export function createAction<Input, Output>(actionRoute: string, entitiesUsed: unknown[]): Action<Input, Output>
