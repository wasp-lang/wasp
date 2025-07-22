import { createCreateJWT, createValidateJWT } from "@wasp.sh/libs-auth";

import { config } from "wasp/server";

const JWT_SECRET = new TextEncoder().encode(config.auth.jwtSecret);
const JWT_ALGORITHM = "HS256";

// PRIVATE API
export const createJWT = createCreateJWT(JWT_SECRET, JWT_ALGORITHM);

// PRIVATE API
export const validateJWT = createValidateJWT(JWT_SECRET, JWT_ALGORITHM);

// PRIVATE API
export { TimeSpan } from "@wasp.sh/libs-auth";
