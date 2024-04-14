import { foo as foo_ext } from 'wasp/ext-src/server/actions/bar';
export type MySpecialAction = typeof foo_ext;
export declare const mySpecialAction: (args: any, context: any) => Promise<any>;
