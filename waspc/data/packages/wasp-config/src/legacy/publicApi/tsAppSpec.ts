import * as AppSpec from "../../appSpec.js";
import { Branded } from "../../branded.js";

export type TsAppSpec = {
  app: { name: string; config: AppConfig };
  actions: Map<string, ActionConfig>;
  apiNamespaces: Map<string, ApiNamespaceConfig>;
  apis: Map<string, ApiConfig>;
  auth?: AuthConfig;
  client?: ClientConfig;
  cruds: Map<string, CrudConfig>;
  db?: DbConfig;
  emailSender?: EmailSenderConfig;
  jobs: Map<string, JobConfig>;
  pages: Map<string, PageConfig>;
  queries: Map<string, QueryConfig>;
  routes: Map<string, RouteConfig>;
  server?: ServerConfig;
  websocket?: WebsocketConfig;
};

export type AppConfig = {
  title: string;
  wasp: AppSpec.Wasp;
  head?: string[];
};

export type ActionConfig = {
  fn: FunctionExtImport;
  entities?: string[];
  auth?: boolean;
};

export type ApiNamespaceConfig = {
  middlewareConfigFn: FunctionExtImport;
  path: string;
};

export type ApiConfig = {
  fn: FunctionExtImport;
  middlewareConfigFn?: FunctionExtImport;
  entities?: string[];
  httpRoute: HttpRoute;
  auth?: boolean;
};

export type HttpRoute = {
  method: AppSpec.HttpMethod;
  route: string;
};

export type AuthConfig = {
  userEntity: string;
  methods: AuthMethods;
  externalAuthEntity?: string;
  onAuthFailedRedirectTo: string;
  onAuthSucceededRedirectTo?: string;
  onBeforeSignup?: FunctionExtImport;
  onAfterSignup?: FunctionExtImport;
  onAfterEmailVerified?: FunctionExtImport;
  onBeforeOAuthRedirect?: FunctionExtImport;
  onBeforeLogin?: FunctionExtImport;
  onAfterLogin?: FunctionExtImport;
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
  userSignupFields?: ObjectExtImport;
};

export type EmailAuthConfig = {
  userSignupFields?: ObjectExtImport;
  fromField: EmailFromField;
  emailVerification: EmailVerificationConfig;
  passwordReset: PasswordResetConfig;
};

export type EmailVerificationConfig = {
  getEmailContentFn?: FunctionExtImport;
  clientRoute: string;
};

export type PasswordResetConfig = {
  getEmailContentFn?: FunctionExtImport;
  clientRoute: string;
};

export type ExternalAuthConfig = {
  configFn?: FunctionExtImport;
  userSignupFields?: ObjectExtImport;
};

export type ClientConfig = {
  rootComponent?: FunctionExtImport;
  setupFn?: FunctionExtImport;
  baseDir?: `/${string}`;
  envValidationSchema?: ObjectExtImport;
};

export type CrudConfig = {
  entity: string;
  operations: CrudOperations;
};

export type CrudOperations = {
  get?: CrudOperationOptions;
  getAll?: CrudOperationOptions;
  create?: CrudOperationOptions;
  update?: CrudOperationOptions;
  delete?: CrudOperationOptions;
};

export type CrudOperationOptions = {
  isPublic?: boolean;
  overrideFn?: FunctionExtImport;
};

export type DbConfig = {
  seeds?: FunctionExtImport[];
  prismaSetupFn?: FunctionExtImport;
};

export type EmailSenderConfig = {
  provider: AppSpec.EmailProvider;
  defaultFrom?: EmailFromField;
};

export type JobConfig = {
  executor: AppSpec.JobExecutor;
  perform: Perform;
  schedule?: Schedule;
  entities?: string[];
};

export type Perform = {
  fn: FunctionExtImport;
  executorOptions?: ExecutorOptions;
};

export type Schedule = {
  cron: string;
  args?: object;
  executorOptions?: ExecutorOptions;
};

export type ExecutorOptions = {
  // TODO: Type this better and test it
  // rewriting waspc/todoApp should make sure it works
  pgBoss: object;
};

export type PageConfig = {
  component: FunctionExtImport;
  authRequired?: boolean;
};

export type QueryConfig = {
  fn: FunctionExtImport;
  entities?: string[];
  auth?: boolean;
};

export type RouteConfig = {
  path: string;
  to: PageName;
  prerender?: boolean;
  lazy?: boolean;
};

export type PageName = Branded<string, "PageName">;

export type ServerConfig = {
  setupFn?: FunctionExtImport;
  middlewareConfigFn?: FunctionExtImport;
  envValidationSchema?: ObjectExtImport;
};

export type WebsocketConfig = {
  fn: FunctionExtImport;
  autoConnect?: boolean;
};

export type ExtImport =
  | {
      import: string;
      from: AppSpec.ExtImport["path"];
      alias?: string;
    }
  | {
      importDefault: string;
      from: AppSpec.ExtImport["path"];
      alias?: string;
    };

// eslint-disable-next-line @typescript-eslint/no-explicit-any
type AnyFunction = (...args: any[]) => any;

type NonDescriptorObject = object & {
  import?: never;
  importDefault?: never;
  from?: never;
  call?: never;
  apply?: never;
  bind?: never;
};

export type FunctionExtImport = ExtImport | AnyFunction;
export type ObjectExtImport = ExtImport | NonDescriptorObject;
export type AuthoredExtImport = FunctionExtImport | ObjectExtImport;

export type EmailFromField = {
  name?: string;
  email: string;
};
