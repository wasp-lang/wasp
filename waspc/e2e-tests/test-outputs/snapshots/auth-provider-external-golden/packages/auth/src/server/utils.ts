import { getAuthContractErrorCode } from "@wasp.sh/auth-contract";
import { TimeSpan, createJWTHelpers } from "@wasp.sh/lib-auth/node";

import { HttpError } from "./http.js";
import type { UserSignupFields, WaspAuthRuntime } from "./types.js";
import { throwValidationError } from "./validation.js";

const sleep = (ms: number): Promise<unknown> =>
  new Promise((r) => setTimeout(r, ms));

// If an user exists, we don't want to leak information about it. Pretending
// that we're doing some work will make it harder for an attacker to determine
// if a user exists or not.
export async function doFakeWork(): Promise<unknown> {
  const timeToWork = Math.floor(Math.random() * 1000) + 1000;
  return sleep(timeToWork);
}

export function createInvalidCredentialsError(message?: string): HttpError {
  return new HttpError(401, "Invalid credentials", { message });
}

function prismaErrorCode(e: unknown): string | null {
  if (typeof e !== "object" || e === null) return null;
  const { code, name } = e as { code?: unknown; name?: unknown };
  return name === "PrismaClientKnownRequestError" && typeof code === "string"
    ? code
    : null;
}

function isPrismaValidationError(e: unknown): boolean {
  return (
    typeof e === "object" &&
    e !== null &&
    (e as { name?: unknown }).name === "PrismaClientValidationError"
  );
}

/** The same error translation the in-tree flows applied, duck-typed on Prisma's error names. */
export function rethrowPossibleAuthError(e: unknown): never {
  if (
    prismaErrorCode(e) === "P2002" ||
    getAuthContractErrorCode(e) === "wasp-auth/duplicate-identity"
  ) {
    throw new HttpError(422, "Save failed", {
      message: `user with the same identity already exists`,
    });
  }
  if (isPrismaValidationError(e)) {
    console.error(e);
    throw new HttpError(422, "Save failed", {
      message: "there was a database error",
    });
  }
  if (prismaErrorCode(e) === "P2021") {
    console.error(e);
    console.info(
      "🐝 This error can happen if you did't run the database migrations.",
    );
    throw new HttpError(500, "Save failed", {
      message: `there was a database error`,
    });
  }
  if (prismaErrorCode(e) === "P2003") {
    console.error(e);
    console.info(`🐝 This error can happen if you have some relation on your user entity
   but you didn't specify the "onDelete" behaviour to either "Cascade" or "SetNull".
   Read more at: https://www.prisma.io/docs/orm/prisma-schema/data-model/relations/referential-actions`);
    throw new HttpError(500, "Save failed", {
      message: `there was a database error`,
    });
  }
  throw e;
}

export async function validateAndGetUserFields(
  data: Record<string, unknown>,
  userSignupFields?: UserSignupFields,
): Promise<Record<string, unknown>> {
  const { password: _password, ...sanitizedData } = data;
  const result: Record<string, unknown> = {};
  if (!userSignupFields) {
    return result;
  }
  for (const [field, getFieldValue] of Object.entries(userSignupFields)) {
    try {
      result[field] = await getFieldValue(sanitizedData);
    } catch (e) {
      throwValidationError((e as Error).message);
    }
  }
  return result;
}

/** The app's JWT helpers, on the secret Wasp hands this provider through its env. */
export function makeJwt(runtime: WaspAuthRuntime) {
  const secret = runtime.env.JWT_SECRET;
  if (secret === undefined) {
    throw new Error("JWT_SECRET is required by Wasp's auth.");
  }
  return createJWTHelpers(new TextEncoder().encode(secret), "HS256");
}

export { TimeSpan };
