import { type AuthenticatedOperationFor } from '../wrappers.js';
import { foo as foo_ext } from 'wasp/ext-src/server/actions/bar';
export type MySpecialAction_ext = typeof foo_ext;
export declare const mySpecialAction: AuthenticatedOperationFor<MySpecialAction_ext>;
