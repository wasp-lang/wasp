import { Query } from "."

export function createQuery<BackendQuery extends GenericBackendQuery>(queryRoute: string, entitiesUsed: any[]): QueryFor<BackendQuery>

type QueryFor<BackendQuery extends GenericBackendQuery> = Query<Parameters<BackendQuery>[0], Awaited<ReturnType<BackendQuery>>>

type GenericBackendQuery = (args: never, context: any) => Promise<unknown>
