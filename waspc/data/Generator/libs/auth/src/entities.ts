/**
 * The app-specific types that Wasp's auth is generated against.
 *
 * The lib never knows the app's entities or relation names. The generated code
 * builds one object type like this from the app's Prisma types and instantiates
 * every generic in this lib with it. Relation names are also passed as values at
 * runtime (see e.g. `AuthIdentitiesConfig`), typed against the members here so the
 * two can't drift apart.
 */
export type AuthEntityTypes = {
  /** The app's user entity, as Prisma types it. */
  user: object;
  /** The `Auth` entity Wasp injects into the schema. */
  auth: { id: string };
  /** The `AuthIdentity` entity Wasp injects into the schema. */
  authIdentity: AuthIdentityEntity;
  /** `Prisma.<User>CreateInput`. */
  userCreateInput: object;
  /** `Prisma.AuthWhereInput`. */
  authWhereInput: object;
  /** Name of the relation field from the user entity to `Auth`. */
  authFieldOnUserEntity: string;
  /** Name of the relation field from `Auth` to the user entity. */
  userFieldOnAuthEntity: string;
  /** Name of the relation field from `Auth` to `AuthIdentity`. */
  identitiesFieldOnAuthEntity: string;
};

/** The columns of the injected `AuthIdentity` entity that the lib reads. */
export type AuthIdentityEntity = {
  providerName: string;
  providerUserId: string;
  providerData: string;
  authId: string;
};
