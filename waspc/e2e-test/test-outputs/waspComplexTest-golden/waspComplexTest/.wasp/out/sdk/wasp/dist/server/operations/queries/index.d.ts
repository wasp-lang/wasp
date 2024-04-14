import { foo as foo_ext } from 'wasp/ext-src/server/queries/bar';
export type MySpecialQuery = typeof foo_ext;
export declare const mySpecialQuery: (args: any, context: any) => Promise<any>;
