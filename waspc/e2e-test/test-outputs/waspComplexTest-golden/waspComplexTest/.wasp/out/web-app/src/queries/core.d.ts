import { type Query } from '.'
import { Route } from '../types';
import type { Expand, _Awaited, _ReturnType } from '../universal/types'

export function createQuery<BackendQuery extends GenericBackendQuery>(
  queryRoute: string,
  entitiesUsed: any[]
): QueryFor<BackendQuery>

export function addMetadataToQuery(
  query: (...args: any[]) => Promise<unknown>,
  metadata: {
    relativeQueryPath: string;
    queryRoute: Route;
    entitiesUsed: string[];
  },
): void

type QueryFor<BackendQuery extends GenericBackendQuery> = Expand<
  Query<Parameters<BackendQuery>[0], _Awaited<_ReturnType<BackendQuery>>>
>

type GenericBackendQuery = (args: never, context: any) => unknown
