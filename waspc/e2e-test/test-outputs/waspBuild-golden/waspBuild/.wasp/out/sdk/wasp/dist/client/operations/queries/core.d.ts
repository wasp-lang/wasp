import { Route } from 'wasp/client';
import type { _Awaited, _ReturnType } from 'wasp/universal/types';
import { type Query } from '../core.js';
export declare function createQuery<BackendQuery extends GenericBackendQuery>(relativeQueryPath: string, entitiesUsed: string[]): QueryFor<BackendQuery>;
export declare function addMetadataToQuery(query: (...args: any[]) => Promise<unknown>, metadata: {
    relativeQueryPath: string;
    queryRoute: Route;
    entitiesUsed: string[];
}): void;
export type QueryFor<BackendQuery extends GenericBackendQuery> = Query<Parameters<BackendQuery>[0], _Awaited<_ReturnType<BackendQuery>>>;
type GenericBackendQuery = (args: never, context: any) => unknown;
export {};
