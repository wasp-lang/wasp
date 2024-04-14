import type { _Awaited, _ReturnType } from 'wasp/universal/types';
import { type Action } from '../core.js';
export declare function createAction<BackendAction extends GenericBackendAction>(relativeActionRoute: string, entitiesUsed: unknown[]): ActionFor<BackendAction>;
export type ActionFor<BackendAction extends GenericBackendAction> = Action<Parameters<BackendAction>[0], _Awaited<_ReturnType<BackendAction>>>;
type GenericBackendAction = (args: never, context: any) => unknown;
export {};
