import { SignJWT, jwtVerify, errors as joseErrors } from "jose";

/**
 * Supported time units for TimeSpan.
 */
type TimeUnit = "s" | "m" | "h" | "d" | "w";

const UNIT_TO_SECONDS: Record<TimeUnit, number> = {
  s: 1,
  m: 60,
  h: 3600,
  d: 86400,
  w: 604800,
};

/**
 * Represents a duration of time, used for JWT expiration.
 * Replacement for oslo's TimeSpan class.
 */
export class TimeSpan {
  /** The duration in seconds. */
  readonly seconds: number;

  constructor(value: number, unit: TimeUnit) {
    this.seconds = value * UNIT_TO_SECONDS[unit];
  }
}

/** JWT payload type — a record of string keys to JSON-compatible values. */
type JWTPayload = Record<string, unknown>;

/** Options for creating a JWT token. */
interface CreateJWTOptions {
  expiresIn: TimeSpan;
}

/**
 * Creates helper functions for creating and validating JWT tokens.
 *
 * @param JWT_SECRET Secret used to sign the JWT token.
 * @param JWT_ALGORITHM Algorithm used to sign the JWT token (e.g. "HS256").
 * @returns An object containing `createJWT` and `validateJWT` functions.
 */
export function createJWTHelpers(
  JWT_SECRET: Uint8Array,
  JWT_ALGORITHM: string,
) {
  return {
    /**
     * Creates a JWT token with the given payload and options.
     * @param data The payload to include in the JWT token.
     * @param options Options including `expiresIn` as a `TimeSpan`.
     * @returns The created JWT token.
     */
    createJWT: (
      data: JWTPayload,
      options: CreateJWTOptions,
    ): Promise<string> => {
      const now = Math.floor(Date.now() / 1000);
      return new SignJWT(data)
        .setProtectedHeader({ alg: JWT_ALGORITHM, typ: "JWT" })
        .setExpirationTime(now + options.expiresIn.seconds)
        .sign(JWT_SECRET);
    },
    /**
     * Validates a JWT token and returns its payload if valid.
     * @param token
     * @returns The payload of the JWT token.
     * @throws If the token is invalid or expired.
     */
    validateJWT: async <T>(token: string): Promise<T> => {
      try {
        const { payload } = await jwtVerify(token, JWT_SECRET, {
          algorithms: [JWT_ALGORITHM],
        });
        return payload as T;
      } catch (e) {
        if (e instanceof joseErrors.JWTExpired) {
          throw new Error("Expired JWT");
        }
        if (e instanceof joseErrors.JWSSignatureVerificationFailed) {
          throw new Error("Invalid signature");
        }
        throw new Error("Unexpected JWT error");
      }
    },
  };
}
