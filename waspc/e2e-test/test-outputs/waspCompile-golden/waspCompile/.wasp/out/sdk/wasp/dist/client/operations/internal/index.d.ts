import { HttpMethod } from 'wasp/client';
export type OperationRoute = {
    method: HttpMethod.Post;
    path: string;
};
export declare function callOperation(operationRoute: OperationRoute, args: any): Promise<unknown>;
export declare function makeOperationRoute(relativeOperationRoute: string): OperationRoute;
//# sourceMappingURL=index.d.ts.map