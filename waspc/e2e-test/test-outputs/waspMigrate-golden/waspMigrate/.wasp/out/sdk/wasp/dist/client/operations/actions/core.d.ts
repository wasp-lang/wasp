import type { OperationRpcFor, GenericBackendOperation } from '../rpc.js';
export declare function createAction<BackendAction extends GenericBackendOperation>(relativeActionRoute: string, entitiesUsed: unknown[]): ActionFor<BackendAction>;
export type ActionFor<BackendAction extends GenericBackendOperation> = OperationRpcFor<BackendAction>;
//# sourceMappingURL=core.d.ts.map