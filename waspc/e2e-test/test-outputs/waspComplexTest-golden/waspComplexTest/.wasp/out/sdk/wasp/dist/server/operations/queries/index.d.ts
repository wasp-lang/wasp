import { foo as __userDefinedFoo } from 'wasp/ext-src/server/queries/bar.js';
export type MySpecialQuery = typeof __userDefinedFoo;
export declare const mySpecialQuery: (args: any, context: any) => Promise<any>;
