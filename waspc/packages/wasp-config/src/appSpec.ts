/** This module is a mirror implementation of FromJSON for AppSpec Decls in
 * TypeScript. The original implemention is in Haskell (waspc).
 *
 * IMPORTANT: Do not change this file without updating the AppSpec in waspc.
 */

export type Decl = {
  [Type in keyof DeclTypeToValue]: {
    declType: Type;
    declName: string;
    declValue: DeclTypeToValue[Type];
  };
}[keyof DeclTypeToValue];

export type DeclTypeToValue = {
  App: App;
  Page: Page;
  Route: Route;
  Query: Query;
  Action: Action;
  Job: Job;
  Api: Api;
  ApiNamespace: ApiNamespace;
  Crud: Crud;
};

export type GetDeclForType<T extends Decl["declType"]> = Extract<
  Decl,
  { declType: T }
>;

// NOTE: Entities are defined in the schema.prisma file, but they can still be
// referenced.
export type DeclType = Decl["declType"] | "Entity";

export type Page = {
  component: ExtImport;
  authRequired: Optional<boolean>;
};

export type Route = {
  path: string;
  to: Ref<"Page">;
};

export type Action = {
  fn: ExtImport;
  entities: Optional<Ref<"Entity">[]>;
  auth: Optional<boolean>;
};

export type Query = {
  fn: ExtImport;
  entities: Optional<Ref<"Entity">[]>;
  auth: Optional<boolean>;
};

export type Job = {
  executor: JobExecutor;
  perform: Perform;
  schedule: Optional<Schedule>;
  entities: Optional<Ref<"Entity">[]>;
};
export type Schedule = {
  cron: string;
  args: Optional<object>;
  executorOptions: Optional<ExecutorOptions>;
};

export type Perform = {
  fn: ExtImport;
  executorOptions: Optional<ExecutorOptions>;
};

export type Api = {
  fn: ExtImport;
  middlewareConfigFn: Optional<ExtImport>;
  entities: Optional<Ref<"Entity">[]>;
  httpRoute: HttpRoute;
  auth: Optional<boolean>;
};

export type ApiNamespace = {
  middlewareConfigFn: ExtImport;
  path: string;
};

export type Crud = {
  entity: Ref<"Entity">;
  operations: CrudOperations;
};

export type App = {
  wasp: Wasp;
  title: string;
  head: Optional<string[]>;
  auth: Optional<Auth>;
  server: Optional<Server>;
  client: Optional<Client>;
  db: Optional<Db>;
  emailSender: Optional<EmailSender>;
  webSocket: Optional<WebSocket>;
};

export type ExtImportKind = "named" | "default";
export type ExtImport = {
  kind: ExtImportKind;
  name: string;
  path: `@src/${string}`;
};

export type JobExecutor = "PgBoss";

export type ExecutorOptions = {
  pgBoss: Optional<object>;
};

export type HttpMethod = "ALL" | "GET" | "POST" | "PUT" | "DELETE";

export type HttpRoute = [HttpMethod, string];

export type CrudOperations = {
  get: Optional<CrudOperationOptions>;
  getAll: Optional<CrudOperationOptions>;
  create: Optional<CrudOperationOptions>;
  update: Optional<CrudOperationOptions>;
  delete: Optional<CrudOperationOptions>;
};

export type CrudOperationOptions = {
  isPublic: Optional<boolean>;
  overrideFn: Optional<ExtImport>;
};

export type Wasp = {
  version: string;
};

export type Auth = {
  userEntity: Ref<"Entity">;
  externalAuthEntity: Optional<Ref<"Entity">>;
  methods: AuthMethods;
  onAuthFailedRedirectTo: string;
  onAuthSucceededRedirectTo: Optional<string>;
  onBeforeSignup: Optional<ExtImport>;
  onAfterSignup: Optional<ExtImport>;
  onAfterEmailVerified: Optional<ExtImport>;
  onBeforeOAuthRedirect: Optional<ExtImport>;
  onBeforeLogin: Optional<ExtImport>;
  onAfterLogin: Optional<ExtImport>;
};

export type AuthMethods = {
  usernameAndPassword: Optional<UsernameAndPasswordConfig>;
  discord: Optional<ExternalAuthConfig>;
  google: Optional<ExternalAuthConfig>;
  gitHub: Optional<ExternalAuthConfig>;
  keycloak: Optional<ExternalAuthConfig>;
  email: Optional<EmailAuthConfig>;
};

export type UsernameAndPasswordConfig = {
  userSignupFields: Optional<ExtImport>;
};

export type ExternalAuthConfig = {
  configFn: Optional<ExtImport>;
  userSignupFields: Optional<ExtImport>;
};

export type EmailAuthConfig = {
  userSignupFields: Optional<ExtImport>;
  fromField: EmailFromField;
  emailVerification: EmailVerificationConfig;
  passwordReset: PasswordResetConfig;
};

export type EmailSender = {
  provider: EmailProvider;
  defaultFrom: Optional<EmailFromField>;
};

export type EmailProvider = "SMTP" | "SendGrid" | "Mailgun" | "Dummy";

export type EmailFromField = {
  name: Optional<string>;
  email: string;
};

export type EmailVerificationConfig = {
  getEmailContentFn: Optional<ExtImport>;
  clientRoute: Ref<"Route">;
};

export type PasswordResetConfig = {
  getEmailContentFn: Optional<ExtImport>;
  clientRoute: Ref<"Route">;
};

export type Ref<T extends DeclType> = {
  name: string;
  declType: T;
};

export type Server = {
  setupFn: Optional<ExtImport>;
  middlewareConfigFn: Optional<ExtImport>;
  envValidationSchema: Optional<ExtImport>;
};

export type Client = {
  setupFn: Optional<ExtImport>;
  rootComponent: Optional<ExtImport>;
  baseDir: Optional<`/${string}`>;
  envValidationSchema: Optional<ExtImport>;
};

export type Db = {
  seeds: Optional<ExtImport[]>;
  prismaSetupFn: Optional<ExtImport>;
};

export type WebSocket = {
  fn: ExtImport;
  autoConnect: Optional<boolean>;
};

/**
 * We use this type for fields that are optional (Maybe) in AppSpec.
 * We do this instead of `someField?:` because we want TypeScript to force us
 * to explicitly set the field to `undefined`.
 *
 * This way, if the AppSpec changes on the Haskell side, we won't forget to
 * implement a proper mapping in TypeScript.
 *
 * For example, let's say `bar` is optional (both for the user and for the app
 * spec). This would be the correct mapping code:
 * ```
 * const { foo, bar } = userConfig
 * const decl: SomeDecl = {
 *   foo: mapForAppSpec(foo),
 *   bar: mapForAppSpec(bar)
 * }
 * ```
 * The code below is wrong. It forgets to map `bar` even though it might exist
 * in `userConfig`:
 * ```
 * const { foo } = userConfig
 * const decl: SomeDecl = {
 *   foo: mapForAppSpec(foo),
 * }
 * ```
 * If `bar` is an optional field of `SomeDecl` (`bar?: string`), TypeScript
 * doesn't catch this error.
 *
 * If `bar` is a mandatory field of `SomeDecl` that can be set to `undefined`
 * (`bar: Optional<string>`), TypeScript catches the error.
 *
 * Explicitly setting optional fields to `undefined` doesn't impact JSON
 * serialization since fields set to `undefined` are treated as missing fields.
 */
type Optional<T> = T | undefined;
