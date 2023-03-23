export type Query<Input, Output> = {
  (args: Input): Promise<Output>
  queryCacheKey: string[]
  route: string
}

export function createQuery<Input, Output>(queryRoute: string, entitiesUsed: any[]): Query<Input, Output>
