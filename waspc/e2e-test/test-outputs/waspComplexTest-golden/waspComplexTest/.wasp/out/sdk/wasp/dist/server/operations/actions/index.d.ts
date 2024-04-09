import { foo as foo__userDefined } from 'wasp/ext-src/server/actions/bar.js';
export type MySpecialAction = typeof foo__userDefined;
export declare const mySpecialAction: (args: any, context: any) => Promise<any>;
