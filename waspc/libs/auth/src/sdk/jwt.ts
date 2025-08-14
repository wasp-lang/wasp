import { type JWTAlgorithm, createJWT, validateJWT } from "oslo/jwt";

export function createJWTHelpers(
  JWT_SECRET: Uint8Array,
  JWT_ALGORITHM: JWTAlgorithm,
) {
  return {
    createJWT: (
      data: Parameters<typeof createJWT>[2],
      options: Parameters<typeof createJWT>[3],
    ): Promise<string> => {
      return createJWT(JWT_ALGORITHM, JWT_SECRET, data, options);
    },
    validateJWT: async <T>(token: string): Promise<T> => {
      const { payload } = await validateJWT(JWT_ALGORITHM, JWT_SECRET, token);
      return payload as T;
    },
  };
}

export { TimeSpan } from "oslo";
