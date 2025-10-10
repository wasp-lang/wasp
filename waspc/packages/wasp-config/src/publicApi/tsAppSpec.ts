import * as AppSpec from "../appSpec.js";
import { Branded } from "../branded.js";

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
  fn: ExtImport;
  entities?: string[];
  auth?: boolean;
};

export type ApiNamespaceConfig = {
  middlewareConfigFn: ExtImport;
  path: string;
};

export type ApiConfig = {
  fn: ExtImport;
  middlewareConfigFn?: ExtImport;
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
  onBeforeSignup?: ExtImport;
  onAfterSignup?: ExtImport;
  onAfterEmailVerified?: ExtImport;
  onBeforeOAuthRedirect?: ExtImport;
  onBeforeLogin?: ExtImport;
  onAfterLogin?: ExtImport;
  sessionLength?: number;
};

export type AuthMethods = {
  usernameAndPassword?: UsernameAndPasswordConfig;
  discord?: ExternalAuthConfig;
  google?: ExternalAuthConfig;
  gitHub?: ExternalAuthConfig;
  keycloak?: ExternalAuthConfig;
  email?: EmailAuthConfig;
};

export type UsernameAndPasswordConfig = {
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

export type ExternalAuthConfig = {
  configFn?: ExtImport;
  userSignupFields?: ExtImport;
};

export type ClientConfig = {
  rootComponent?: ExtImport;
  setupFn?: ExtImport;
  baseDir?: `/${string}`;
  envValidationSchema?: ExtImport;
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
  overrideFn?: ExtImport;
};

export type DbConfig = {
  seeds?: ExtImport[];
  prismaSetupFn?: ExtImport;
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
  fn: ExtImport;
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
  component: ExtImport;
  authRequired?: boolean;
};

export type QueryConfig = {
  fn: ExtImport;
  entities?: string[];
  auth?: boolean;
};

export type RouteConfig = {
  path: string;
  to: PageName;
};

export type PageName = Branded<string, "PageName">;

export type ServerConfig = {
  setupFn?: ExtImport;
  middlewareConfigFn?: ExtImport;
  envValidationSchema?: ExtImport;
};

export type WebsocketConfig = {
  fn: ExtImport;
  autoConnect?: boolean;
};

export type ExtImport =
  | {
      import: string;
      from: AppSpec.ExtImport["path"];
    }
  | {
      importDefault: string;
      from: AppSpec.ExtImport["path"];
    };

export type EmailFromField = {
  name?: string;
  email: string;
};
