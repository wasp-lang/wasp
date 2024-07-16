import { type AuthenticatedOperationFor } from '../wrappers.js';
import { foo as foo_ext } from 'wasp/ext-src/server/queries/bar';
export type MySpecialQuery_ext = typeof foo_ext;
export declare const mySpecialQuery: AuthenticatedOperationFor<MySpecialQuery_ext>;
