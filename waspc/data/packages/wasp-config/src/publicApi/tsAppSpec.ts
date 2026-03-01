import * as AppSpec from "../appSpec.js";
import { Branded } from "../branded.js";

// The declarations a Module can hold (things with names, can have multiples)
export type TsModuleSpec = {
  actions: Map<string, ActionConfig>;
  apiNamespaces: Map<string, ApiNamespaceConfig>;
  apis: Map<string, ApiConfig>;
  cruds: Map<string, CrudConfig>;
  jobs: Map<string, JobConfig>;
  pages: Map<string, PageConfig>;
  queries: Map<string, QueryConfig>;
  routes: Map<string, RouteConfig>;
  serverSetupFn?: ExtImport;
  clientSetupFn?: ExtImport;
  provides: Map<string, JsonSerializable>;
  packageName?: string;
  entityAliases: Set<string>;
  entityDeclarations: Map<string, { fields: Record<string, string> }>;
  requiresAuth?: boolean;
};

export type ModuleProvideEntry = {
  packageName: string;
  values: Record<string, JsonSerializable>;
};

export type ModuleEntityMapEntry = {
  packageName: string;
  entityMap: Record<string, string>;
};

// Full app spec = module declarations + app-level singletons
export type TsAppSpec = TsModuleSpec & {
  app: { name: string; config: AppConfig };
  auth?: AuthConfig;
  client?: ClientConfig;
  db?: DbConfig;
  emailSender?: EmailSenderConfig;
  server?: ServerConfig;
  websocket?: WebsocketConfig;
  moduleServerSetupFns: ExtImport[];
  moduleClientSetupFns: ExtImport[];
  moduleProvides: ModuleProvideEntry[];
  moduleEntityMaps: ModuleEntityMapEntry[];
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

export type JsonSerializable =
  | string
  | number
  | boolean
  | null
  | JsonSerializable[]
  | { [key: string]: JsonSerializable };
