import { type HttpMethod } from "../types";

export type Query<Input, Output> = {
  (args: Input): Promise<Output>
  queryCacheKey: string[]
  route: { method: HttpMethod, path: string }
}

export function createQuery<Input, Output>(queryRoute: string, entitiesUsed: any[]): Query<Input, Output>
