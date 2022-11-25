export type Action<Input, Output> = (args?: Input) => Promise<Output>;

export function createAction<Input, Output>(actionRoute: string, entitiesUsed: unknown[]): Action<Input, Output>
