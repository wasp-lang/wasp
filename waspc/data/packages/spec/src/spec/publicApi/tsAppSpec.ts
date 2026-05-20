import type { RequireOneOrNone } from "type-fest";
import * as AppSpec from "../../appSpec.js";
import type { AnyFunction } from "../../typeUtils.js";
import type { ExtImport } from "../extImport.js";

export type App = {
  name: string;
  wasp: AppSpec.Wasp;
  title: string;
  head?: string[];
  auth?: Auth;
  server?: Server;
  client?: Client;
  db?: Db;
  emailSender?: EmailSender;
  webSocket?: WebSocket;
  parts: Part[];
};

export type Auth = AuthHooks & {
  userEntity: string;
  externalAuthEntity?: string;
  methods: AuthMethods;
  onAuthFailedRedirectTo: string;
  onAuthSucceededRedirectTo?: string;
};

export type AuthHooks = Partial<Record<AuthHookName, ExtImport | AnyFunction>>;

export type AuthHookName =
  | "onBeforeSignup"
  | "onAfterSignup"
  | "onAfterEmailVerified"
  | "onBeforeOAuthRedirect"
  | "onBeforeLogin"
  | "onAfterLogin";

export type AuthMethods = Partial<
  Record<SocialAuthMethodName, ExternalAuthConfig>
> &
  RequireOneOrNone<{
    usernameAndPassword: UsernameAndPasswordConfig;
    email: EmailAuthConfig;
  }>;

export type SocialAuthMethodName =
  | "discord"
  | "google"
  | "gitHub"
  | "keycloak"
  | "microsoft";

export type UsernameAndPasswordConfig = BaseAuthMethodConfig;

export type ExternalAuthConfig = BaseAuthMethodConfig & {
  configFn?: ExtImport | AnyFunction;
};

export type EmailAuthConfig = BaseAuthMethodConfig & {
  fromField: EmailFromField;
  emailVerification: EmailFlowConfig;
  passwordReset: EmailFlowConfig;
};

export type BaseAuthMethodConfig = {
  userSignupFields?: ExtImport | AnyFunction;
};

export type EmailFlowConfig = {
  getEmailContentFn?: ExtImport | AnyFunction;
  clientRoute: string;
};

export type Server = {
  setupFn?: ExtImport | AnyFunction;
  middlewareConfigFn?: ExtImport | AnyFunction;
  envValidationSchema?: ExtImport | ZodSchema;
};

export type Client = {
  rootComponent?: ExtImport | AnyFunction;
  setupFn?: ExtImport | AnyFunction;
  baseDir?: `/${string}`;
  envValidationSchema?: ExtImport | ZodSchema;
};

export type Db = {
  seeds?: (ExtImport | AnyFunction)[];
  prismaSetupFn?: ExtImport | AnyFunction;
};

export type EmailSender = {
  provider: AppSpec.EmailProvider;
  defaultFrom?: EmailFromField;
};

export type EmailFromField = {
  name?: string;
  email: string;
};

export type WebSocket = {
  fn: ExtImport | AnyFunction;
  autoConnect?: boolean;
};

export type Part =
  | Page
  | Route
  | Query
  | Action
  | Api
  | ApiNamespace
  | Job
  | Crud;

export type Page = MakePart<
  "page",
  {
    component: ExtImport | AnyFunction;
    authRequired?: boolean;
  }
>;

export type Route = MakePart<
  "route",
  {
    name: string;
    path: string;
    page: Page;
    prerender?: boolean;
    lazy?: boolean;
  }
>;

export type Query = MakePart<
  "query",
  {
    fn: ExtImport | AnyFunction;
    entities?: string[];
    auth?: boolean;
  }
>;

export type Action = MakePart<
  "action",
  {
    fn: ExtImport | AnyFunction;
    entities?: string[];
    auth?: boolean;
  }
>;

export type Api = MakePart<
  "api",
  {
    method: HttpMethod;
    path: string;
    fn: ExtImport | AnyFunction;
    middlewareConfigFn?: ExtImport | AnyFunction;
    entities?: string[];
    auth?: boolean;
  }
>;

export type ApiNamespace = MakePart<
  "apiNamespace",
  {
    middlewareConfigFn: ExtImport | AnyFunction;
    path: string;
  }
>;

export type HttpMethod = "ALL" | "GET" | "POST" | "PUT" | "DELETE";

export type Job = MakePart<
  "job",
  {
    fn: ExtImport | AnyFunction;
    executor: JobExecutor;
    schedule?: Schedule;
    entities?: string[];
    performExecutorOptions?: ExecutorOptions;
  }
>;

export type JobExecutor = "PgBoss";

export type Schedule = {
  cron: string;
  args?: object;
  executorOptions?: ExecutorOptions;
};

export type ExecutorOptions = {
  pgBoss: object;
};

export type Crud = MakePart<
  "crud",
  {
    name: string;
    entity: string;
    operations: CrudOperations;
  }
>;

export type CrudOperations = Partial<
  Record<CrudOperation, CrudOperationOptions>
>;

export type CrudOperation = "get" | "getAll" | "create" | "update" | "delete";

export type CrudOperationOptions = {
  isPublic?: boolean;
  overrideFn?: ExtImport;
};

export type {
  DefaultExtImport,
  ExtImport,
  NamedExtImport,
} from "../extImport.js";

/**
 * We need the kind to differentiate between parts with the same structure. One
 * example is queries and actions.
 *
 * Imagine the situation if we didn't have kinds. How would TS know which one
 * is an action and which one a query?
 *   { fn: { import: "getFoo" from: "@src/foo" }, auth: true }
 *   { fn: { import: "setFoo" from: "@src/foo" }, auth: true }
 *
 * There are likely other examples where some combination of mandatory and
 * optional fields makes two parts structurally identical.
 */
export type MakePart<Kind extends string, PartConfig> = {
  kind: Kind;
} & PartConfig;

/**
 * Imported ref env schemas are lowered to ExtImport objects before mapping,
 * but the user still sees the public API that allows the runtime Zod object
 * (in `server.envValidationSchema` and `client.envValidationSchema`).
 * To avoid needing to depend on `zod` package, we structurally recognize Zod 4
 * schemas via their documented library-author marker.
 * See https://zod.dev/library-authors.
 */
export type ZodSchema = {
  readonly _zod: {
    readonly def: object;
  };
};
