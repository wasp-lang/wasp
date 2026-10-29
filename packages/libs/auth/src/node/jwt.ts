import { type JWTAlgorithm, createJWT, validateJWT } from "oslo/jwt";

/**
 * Creates helper functions for creating and validating JWT tokens.
 *
 * @param JWT_SECRET Secret used to sign the JWT token.
 * @param JWT_ALGORITHM Algorithm used to sign the JWT token.
 * @returns An object containing `createJWT` and `validateJWT` functions.
 */
export function createJWTHelpers(
  JWT_SECRET: Uint8Array,
  JWT_ALGORITHM: JWTAlgorithm,
) {
  return {
    /**
     * Creates a JWT token with the given payload and options.
     * @param data The payload to include in the JWT token.
     * @param options Additional options for `oslo/jwt`'s `createJWT` function.
     * @returns The created JWT token.
     */
    createJWT: (
      data: Parameters<typeof createJWT>[2],
      options: Parameters<typeof createJWT>[3],
    ): Promise<string> => {
      return createJWT(JWT_ALGORITHM, JWT_SECRET, data, options);
    },
    /**
     * Validates a JWT token and returns its payload if valid.
     * @param token
     * @returns The payload of the JWT token.
     * @throws If the token is invalid or expired.
     */
    validateJWT: async <T>(token: string): Promise<T> => {
      const { payload } = await validateJWT(JWT_ALGORITHM, JWT_SECRET, token);
      return payload as T;
    },
  };
}

export { TimeSpan } from "oslo";
