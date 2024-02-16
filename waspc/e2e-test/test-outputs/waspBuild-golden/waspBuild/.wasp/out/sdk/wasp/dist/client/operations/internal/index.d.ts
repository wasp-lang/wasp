import { HttpMethod } from 'wasp/client';
export type OperationRoute = {
    method: HttpMethod;
    path: string;
};
export declare function callOperation(operationRoute: OperationRoute & {
    method: HttpMethod.Post;
}, args: any): Promise<unknown>;
export declare function makeOperationRoute(relativeOperationRoute: string): OperationRoute;
