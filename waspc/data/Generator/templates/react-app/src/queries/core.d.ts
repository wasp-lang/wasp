export function createQuery<BackendQuery extends GenericBackendQuery>(queryRoute: string, entitiesUsed: any[]): QueryFor<BackendQuery>

type QueryFor<BackendQuery extends GenericBackendQuery> = (queryKey: string[], args: Parameters<BackendQuery>[0]) => ReturnType<BackendQuery>

type GenericBackendQuery = (args: never, context: any) => Promise<unknown>