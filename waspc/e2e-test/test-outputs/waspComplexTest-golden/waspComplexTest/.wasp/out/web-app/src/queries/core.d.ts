export type Query<Input, Output> = (args: Input) => Promise<Output>

export function createQuery<Input, Output>(queryRoute: string, entitiesUsed: any[]): Query<Input, Output>
