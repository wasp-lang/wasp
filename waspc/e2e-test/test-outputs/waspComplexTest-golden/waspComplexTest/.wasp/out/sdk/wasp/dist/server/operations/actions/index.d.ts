import { foo as __userDefinedFoo } from 'wasp/ext-src/server/actions/bar.js';
export type MySpecialAction = typeof __userDefinedFoo;
export declare const mySpecialAction: (args: any, context: any) => Promise<any>;
