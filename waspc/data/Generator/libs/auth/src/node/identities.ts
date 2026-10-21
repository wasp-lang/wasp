// The generated Prisma client throws the error classes of this module, so
// `instanceof` checks against them work (the client is generated into
// `node_modules/.prisma/client`, which requires `@prisma/client/runtime`).
import {
  PrismaClientKnownRequestError,
  PrismaClientValidationError,
} from "@prisma/client/runtime/library";
import { HttpError, getErrorMessage } from "@wasp.sh/lib-sdk-core";
import type { AuthEntityTypes } from "../shared/entities.js";
import { throwValidationError } from "../shared/errors.js";
import type {
  PossibleProviderData,
  ProviderId,
  ProviderName,
} from "../shared/providerData.js";
import {
  ensurePasswordIsHashed,
  serializeProviderData,
} from "./providerDataStorage.js";

/**
 * The subset of a Prisma model delegate the lib calls. The arguments are built
 * with the app's relation names at runtime, so they can't be typed against the
 * app's Prisma types here; the results are typed by the `AuthEntityTypes` the
 * generated code instantiates the lib with.
 */
export type PrismaModelDelegate = {
  findUnique: (args: any) => PromiseLike<any>;
  findFirst: (args: any) => PromiseLike<any>;
  create: (args: any) => PromiseLike<any>;
  update: (args: any) => PromiseLike<any>;
  deleteMany: (args: any) => PromiseLike<any>;
};

export type AuthIdentitiesConfig<T extends AuthEntityTypes> = {
  prisma: {
    user: PrismaModelDelegate;
    auth: PrismaModelDelegate;
    authIdentity: PrismaModelDelegate;
  };
  /** Used in error messages shown to the developer. */
  userEntityName: string;
  authFieldOnUserEntityName: T["authFieldOnUserEntity"];
  userFieldOnAuthEntityName: T["userFieldOnAuthEntity"];
  identitiesFieldOnAuthEntityName: T["identitiesFieldOnAuthEntity"];
};

export type FindAuthWithUserResult<T extends AuthEntityTypes> = T["auth"] & {
  [K in T["userFieldOnAuthEntity"]]: T["user"];
};

export type CreateUserResult<T extends AuthEntityTypes> = T["user"] & {
  [K in T["authFieldOnUserEntity"]]: T["auth"] | null;
};

export type PossibleUserFields<T extends AuthEntityTypes> = Partial<
  T["userCreateInput"]
>;

/**
 * Getters for the user entity's fields, keyed by field name, that run on the
 * data a signup request carries.
 */
export type UserSignupFields = {
  [field: string]: FieldGetter;
};

type FieldGetter = (data: { [key: string]: unknown }) => unknown;

/**
 * An `Auth` row and its user, without the app's relation name in the shape, so
 * code that doesn't know the relation names (the auth providers) can use it.
 */
export type AuthWithUser<T extends AuthEntityTypes> = {
  auth: T["auth"];
  user: T["user"];
};

export type AuthIdentities<T extends AuthEntityTypes> = {
  findAuthIdentity: (
    providerId: ProviderId,
  ) => Promise<T["authIdentity"] | null>;
  updateAuthIdentityProviderData: <PN extends ProviderName>(
    providerId: ProviderId,
    existingProviderData: PossibleProviderData[PN],
    providerDataUpdates: Partial<PossibleProviderData[PN]>,
  ) => Promise<T["authIdentity"]>;
  findAuthWithUserBy: (
    where: T["authWhereInput"],
  ) => Promise<FindAuthWithUserResult<T> | null>;
  findAuthWithUserById: (authId: string) => Promise<AuthWithUser<T> | null>;
  createUser: (
    providerId: ProviderId,
    serializedProviderData?: string,
    userFields?: PossibleUserFields<T>,
  ) => Promise<CreateUserResult<T>>;
  /** Reads the `Auth` row `createUser` included on the created user. */
  getUserAuth: (user: CreateUserResult<T>) => T["auth"] | null;
  deleteUserByAuthId: (authId: string) => Promise<{ count: number }>;
  rethrowPossibleAuthError: (e: unknown) => void;
  validateAndGetUserFields: (
    data: { [key: string]: unknown },
    userSignupFields?: UserSignupFields,
  ) => Promise<Record<string, any>>;
};

/**
 * Creates the functions that look up and create auth identities and users in
 * the app's database.
 */
export function createAuthIdentities<T extends AuthEntityTypes>(
  config: AuthIdentitiesConfig<T>,
): AuthIdentities<T> {
  const {
    prisma,
    authFieldOnUserEntityName,
    userFieldOnAuthEntityName,
    identitiesFieldOnAuthEntityName,
  } = config;

  async function findAuthIdentity(
    providerId: ProviderId,
  ): Promise<T["authIdentity"] | null> {
    return prisma.authIdentity.findUnique({
      where: {
        providerName_providerUserId: providerId,
      },
    });
  }

  /**
   * Updates the provider data for the given auth identity.
   *
   * This function performs data sanitization and serialization.
   * Sanitization is done by hashing the password, so this function
   * expects the password received in the `providerDataUpdates`
   * **not to be hashed**.
   */
  async function updateAuthIdentityProviderData<PN extends ProviderName>(
    providerId: ProviderId,
    existingProviderData: PossibleProviderData[PN],
    providerDataUpdates: Partial<PossibleProviderData[PN]>,
  ): Promise<T["authIdentity"]> {
    // We are doing the sanitization here only on updates to avoid
    // hashing the password multiple times.
    const sanitizedProviderDataUpdates =
      await ensurePasswordIsHashed(providerDataUpdates);
    const newProviderData = {
      ...existingProviderData,
      ...sanitizedProviderDataUpdates,
    };
    const serializedProviderData = serializeProviderData<PN>(newProviderData);
    return prisma.authIdentity.update({
      where: {
        providerName_providerUserId: providerId,
      },
      data: { providerData: serializedProviderData },
    });
  }

  async function findAuthWithUserBy(
    where: T["authWhereInput"],
  ): Promise<FindAuthWithUserResult<T> | null> {
    const result = await prisma.auth.findFirst({
      where,
      include: { [userFieldOnAuthEntityName]: true },
    });

    if (result === null) {
      return null;
    }

    if (result[userFieldOnAuthEntityName] === null) {
      return null;
    }

    return {
      ...result,
      [userFieldOnAuthEntityName]: result[userFieldOnAuthEntityName],
    };
  }

  async function findAuthWithUserById(
    authId: string,
  ): Promise<AuthWithUser<T> | null> {
    const auth = await findAuthWithUserBy({
      id: authId,
    } as T["authWhereInput"]);
    if (auth === null) {
      return null;
    }
    const { [userFieldOnAuthEntityName]: user, ...authFields } = auth as Record<
      string,
      unknown
    >;
    return { auth: authFields as T["auth"], user: user as T["user"] };
  }

  function getUserAuth(user: CreateUserResult<T>): T["auth"] | null {
    return (user as Record<string, unknown>)[authFieldOnUserEntityName] as
      | T["auth"]
      | null;
  }

  async function createUser(
    providerId: ProviderId,
    serializedProviderData?: string,
    userFields?: PossibleUserFields<T>,
  ): Promise<CreateUserResult<T>> {
    return prisma.user.create({
      data: {
        // Using any here to prevent type errors when userFields are not
        // defined. We want Prisma to throw an error in that case.
        ...(userFields ?? ({} as any)),
        [authFieldOnUserEntityName]: {
          create: {
            [identitiesFieldOnAuthEntityName]: {
              create: {
                providerName: providerId.providerName,
                providerUserId: providerId.providerUserId,
                providerData: serializedProviderData,
              },
            },
          },
        },
      },
      // We need to include the Auth entity here because we need `authId`
      // to be able to create a session.
      include: {
        [authFieldOnUserEntityName]: true,
      },
    });
  }

  async function deleteUserByAuthId(
    authId: string,
  ): Promise<{ count: number }> {
    return prisma.user.deleteMany({
      where: {
        [authFieldOnUserEntityName]: {
          id: authId,
        },
      },
    });
  }

  function rethrowPossibleAuthError(e: unknown): void {
    // Prisma code P2002 is for unique constraint violations.
    if (e instanceof PrismaClientKnownRequestError && e.code === "P2002") {
      throw new HttpError(422, "Save failed", {
        message: `user with the same identity already exists`,
      });
    }

    if (e instanceof PrismaClientValidationError) {
      // NOTE: Logging the error since this usually means that there are
      // required fields missing in the request, we want the developer
      // to know about it.
      console.error(e);
      throw new HttpError(422, "Save failed", {
        message: "there was a database error",
      });
    }

    // Prisma code P2021 is for missing table errors.
    if (e instanceof PrismaClientKnownRequestError && e.code === "P2021") {
      // NOTE: Logging the error since this usually means that the database
      // migrations weren't run, we want the developer to know about it.
      console.error(e);
      console.info(
        "🐝 This error can happen if you did't run the database migrations.",
      );
      throw new HttpError(500, "Save failed", {
        message: `there was a database error`,
      });
    }

    // Prisma code P2003 is for foreign key constraint failure
    if (e instanceof PrismaClientKnownRequestError && e.code === "P2003") {
      console.error(e);
      console.info(`🐝 This error can happen if you have some relation on your ${config.userEntityName} entity
   but you didn't specify the "onDelete" behaviour to either "Cascade" or "SetNull".
   Read more at: https://www.prisma.io/docs/orm/prisma-schema/data-model/relations/referential-actions`);
      throw new HttpError(500, "Save failed", {
        message: `there was a database error`,
      });
    }

    throw e;
  }

  async function validateAndGetUserFields(
    data: {
      [key: string]: unknown;
    },
    userSignupFields?: UserSignupFields,
  ): Promise<Record<string, any>> {
    const { password: _password, ...sanitizedData } = data;
    const result: Record<string, any> = {};

    if (!userSignupFields) {
      return result;
    }

    for (const [field, getFieldValue] of Object.entries(userSignupFields)) {
      try {
        const value = await getFieldValue(sanitizedData);
        result[field] = value;
      } catch (e: unknown) {
        throwValidationError(getErrorMessage(e));
      }
    }
    return result;
  }

  return {
    findAuthIdentity,
    updateAuthIdentityProviderData,
    findAuthWithUserBy,
    findAuthWithUserById,
    createUser,
    getUserAuth,
    deleteUserByAuthId,
    rethrowPossibleAuthError,
    validateAndGetUserFields,
  };
}
