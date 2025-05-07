import * as jwt from 'oslo/jwt';
export declare function createJWT(data: Parameters<typeof jwt.createJWT>[2], options: Parameters<typeof jwt.createJWT>[3]): Promise<string>;
export declare function validateJWT<Payload>(token: string): Promise<Payload>;
export { TimeSpan } from 'oslo';
//# sourceMappingURL=jwt.d.ts.map