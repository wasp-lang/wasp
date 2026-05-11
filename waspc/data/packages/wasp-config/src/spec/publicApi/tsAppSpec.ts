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

export type Auth = {
  userEntity: string;
  externalAuthEntity?: string;
  methods: AuthMethods;
  onAuthFailedRedirectTo: string;
  onAuthSucceededRedirectTo?: string;
  onBeforeSignup?: ExtImport;
  onAfterSignup?: ExtImport;
  onAfterEmailVerified?: ExtImport;
  onBeforeOAuthRedirect?: ExtImport;
  onBeforeLogin?: ExtImport;
  onAfterLogin?: ExtImport;
};

export type AuthMethods = {
  usernameAndPassword?: UsernameAndPasswordConfig;
  discord?: ExternalAuthConfig;
  google?: ExternalAuthConfig;
  gitHub?: ExternalAuthConfig;
  keycloak?: ExternalAuthConfig;
  microsoft?: ExternalAuthConfig;
  email?: EmailAuthConfig;
};

export type UsernameAndPasswordConfig = {
  userSignupFields?: ExtImport;
};

export type ExternalAuthConfig = {
  configFn?: ExtImport;
  userSignupFields?: ExtImport;
};

export type EmailAuthConfig = {
  userSignupFields?: ExtImport;
  fromField: EmailFromField;
  emailVerification: EmailVerificationConfig;
  passwordReset: PasswordResetConfig;
};

export type EmailVerificationConfig = {
  getEmailContentFn?: ExtImport;
  clientRoute: string;
};

export type PasswordResetConfig = {
  getEmailContentFn?: ExtImport;
  clientRoute: string;
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

export type Part = Page | Route | Query | Action | Api | ApiNamespace | Job;

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
