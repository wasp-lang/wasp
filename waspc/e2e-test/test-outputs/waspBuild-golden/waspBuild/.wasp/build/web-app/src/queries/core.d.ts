import { type Query } from './index'

export function createQuery<Input, Output>(queryRoute: string, entitiesUsed: any[]): Query<Input, Output>
