import { type JWTAlgorithm, createJWT, validateJWT } from "oslo/jwt";

export const createCreateJWT = (
  JWT_SECRET: Uint8Array,
  JWT_ALGORITHM: JWTAlgorithm,
) => {
  return (
    data: Parameters<typeof createJWT>[2],
    options: Parameters<typeof createJWT>[3],
  ): Promise<string> => {
    return createJWT(JWT_ALGORITHM, JWT_SECRET, data, options);
  };
};

export const createValidateJWT = (
  JWT_SECRET: Uint8Array,
  JWT_ALGORITHM: JWTAlgorithm,
) => {
  return async <Payload>(token: string): Promise<Payload> => {
    const { payload } = await validateJWT(JWT_ALGORITHM, JWT_SECRET, token);
    return payload as Payload;
  };
};

export { TimeSpan } from "oslo";
