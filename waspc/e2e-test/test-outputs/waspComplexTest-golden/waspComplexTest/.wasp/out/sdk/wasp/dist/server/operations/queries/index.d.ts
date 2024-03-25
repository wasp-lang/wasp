import { foo as foo__userDefined } from 'wasp/ext-src/server/queries/bar.js';
export type MySpecialQuery = typeof foo__userDefined;
export declare const mySpecialQuery: (args: any, context: any) => Promise<any>;
