import * as AppSpec from "../../appSpec.js";

export type App<Parts extends readonly Part<string>[]> = {
  name: string;
  wasp: AppSpec.Wasp;
  title: string;
  head?: string[];
  auth?: Auth<RoutesFromParts<Parts>>;
  server?: Server;
  client?: Client;
  db?: Db;
  emailSender?: EmailSender;
  webSocket?: WebSocket;
  parts: Parts;
};

type RoutesFromParts<Parts extends readonly Part<string>[]> =
  Parts extends readonly Part<infer Routes>[] ? Routes : never;

export type Auth<Routes extends string> = AuthHooks & {
  userEntity: string;
  externalAuthEntity?: string;
  methods: AuthMethods<Routes>;
  onAuthFailedRedirectTo: Routes;
  onAuthSucceededRedirectTo?: Routes;
};

export type AuthHooks = Partial<Record<AuthHookName, ExtImport>>;

export type AuthHookName =
  | "onBeforeSignup"
  | "onAfterSignup"
  | "onAfterEmailVerified"
  | "onBeforeOAuthRedirect"
  | "onBeforeLogin"
  | "onAfterLogin";

export type AuthMethods<Routes extends string> = Partial<
  Record<SocialAuthMethodName, ExternalAuthConfig>
> & {
  usernameAndPassword?: UsernameAndPasswordConfig;
  email?: EmailAuthConfig<Routes>;
};

export type SocialAuthMethodName =
  | "discord"
  | "google"
  | "gitHub"
  | "keycloak"
  | "microsoft";

export type UsernameAndPasswordConfig = BaseAuthMethodConfig;

export type ExternalAuthConfig = BaseAuthMethodConfig & {
  configFn?: ExtImport;
};

export type EmailAuthConfig<Routes extends string> = BaseAuthMethodConfig & {
  fromField: EmailFromField;
  emailVerification: EmailFlowConfig<Routes>;
  passwordReset: EmailFlowConfig<Routes>;
};

export type BaseAuthMethodConfig = {
  userSignupFields?: ExtImport;
};

export type EmailFlowConfig<Routes extends string> = {
  getEmailContentFn?: ExtImport;
  clientRoute: Routes;
};

export type Server = {
  setupFn?: ExtImport;
  middlewareConfigFn?: ExtImport;
  envValidationSchema?: ExtImport;
};

export type Client = {
  rootComponent?: ExtImport;
  setupFn?: ExtImport;
  baseDir?: `/${string}`;
  envValidationSchema?: ExtImport;
};

export type Db = {
  seeds?: ExtImport[];
  prismaSetupFn?: ExtImport;
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
  fn: ExtImport;
  autoConnect?: boolean;
};

export type Part<Path extends string> =
  | Page
  | Route<Path>
  | Query
  | Action
  | Api<Path>
  | ApiNamespace
  | Job;

export type Page = MakePart<
  "page",
  {
    component: ExtImport;
    authRequired?: boolean;
  }
>;

export type Route<Path extends string> = MakePart<
  "route",
  {
    name: string;
    path: Path;
    page: Page;
    prerender?: boolean;
    lazy?: boolean;
  }
>;

export type Query = MakePart<
  "query",
  {
    fn: ExtImport;
    entities?: string[];
    auth?: boolean;
  }
>;

export type Action = MakePart<
  "action",
  {
    fn: ExtImport;
    entities?: string[];
    auth?: boolean;
  }
>;

export type Api<Path extends string> = MakePart<
  "api",
  {
    method: HttpMethod;
    path: Path;
    fn: ExtImport;
    middlewareConfigFn?: ExtImport;
    entities?: string[];
    auth?: boolean;
  }
>;

export type ApiNamespace = MakePart<
  "apiNamespace",
  {
    middlewareConfigFn: ExtImport;
    path: string;
  }
>;

export type HttpMethod = "ALL" | "GET" | "POST" | "PUT" | "DELETE";

export type Job = MakePart<
  "job",
  {
    fn: ExtImport;
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

export type ExtImport = NamedExtImport | DefaultExtImport;
export interface NamedExtImport {
  import: string;
  alias?: string;
  from: `@src/${string}`;
}
export interface DefaultExtImport {
  importDefault: string;
  from: `@src/${string}`;
}

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
