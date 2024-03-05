import * as jwt from 'oslo/jwt';
import { config } from 'wasp/server';
const JWT_SECRET = new TextEncoder().encode(config.auth.jwtSecret);
const JWT_ALGORITHM = 'HS256';
// PRIVATE API
export function createJWT(data, options) {
    return jwt.createJWT(JWT_ALGORITHM, JWT_SECRET, data, options);
}
// PRIVATE API
export async function validateJWT(token) {
    const { payload } = await jwt.validateJWT(JWT_ALGORITHM, JWT_SECRET, token);
    return payload;
}
// PRIVATE API
export { TimeSpan } from 'oslo';
//# sourceMappingURL=jwt.js.map