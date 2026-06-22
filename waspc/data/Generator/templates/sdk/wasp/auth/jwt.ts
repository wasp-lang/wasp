import { createJWTHelpers } from "@wasp.sh/lib-auth/node";

import { config } from "../server/index.js";

const JWT_SECRET = new TextEncoder().encode(config.auth.jwtSecret);
const JWT_ALGORITHM = "HS256";

// PRIVATE API
export const { createJWT, validateJWT } = createJWTHelpers(
  JWT_SECRET,
  JWT_ALGORITHM,
);

// PRIVATE API
export { TimeSpan } from "@wasp.sh/lib-auth/node";
