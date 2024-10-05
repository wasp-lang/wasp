import * as AppSpec from './appSpec.js'

export type WaspConfig = AppSpec.Wasp

export type AppConfig = Pick<AppSpec.App, 'title' | 'wasp' | 'head'>

export type ExtImport = {
  import: string
  from: AppSpec.ExtImport['path']
}

export type ServerConfig = {
  setupFn: ExtImport
  middlewareConfigFn: ExtImport
}

export type PageConfig = {
  component: ExtImport
  authRequired?: boolean
}

export type WebsocketConfig = {
  fn: ExtImport
  autoConnect?: boolean
}

export type ClientConfig = {
  rootComponent: ExtImport
  setupFn: ExtImport
}

export type DbConfig = {
  seeds?: ExtImport[]
}

export type RouteConfig = {
  path: string
  to: PageName
}

// Do the trick with brands and symbols
const pageBrand = Symbol('Page')

type PageName = string & { _brand: typeof pageBrand }

// TODO: I have to pull this out of the schema.prisma somehow
type Entity = string

export type ActionConfig = {
  fn: ExtImport
  entities: Entity[]
  auth?: boolean
}

export type ApiNamespaceConfig = {
  middlewareConfigFn: ExtImport
  path: string
}

export type ApiConfig = {
  fn: ExtImport
  middlewareConfigFn?: ExtImport
  entities: Entity[]
  httpRoute: AppSpec.HttpRoute[]
  auth?: boolean
}

export type JobConfig = {
  executor: AppSpec.JobExecutor
  perform: Perform
  schedule?: ScheduleConfig
  entities: Entity[]
}

export type Crud = {
  entity: string
  operations: CrudOperations
}

export type CrudOperations = {
  get?: CrudOperationOptions
  getAll?: CrudOperationOptions
  create?: CrudOperationOptions
  update?: CrudOperationOptions
  delete?: CrudOperationOptions
}

export type CrudOperationOptions = {
  isPublic?: boolean
  overrideFn?: ExtImport
}

export type Perform = {
  fn: ExtImport
  executorOptions?: ExecutorOptions
}

export type ScheduleConfig = {
  cron: string
  args?: object
  executorOptions?: ExecutorOptions
}

type ExecutorOptions = {
  // TODO: Type this better
  pgBoss: object
}

export type QueryConfig = {
  fn: ExtImport
  entities: Entity[]
  auth: boolean
}

export type EmailSenderConfig = AppSpec.EmailSender

export type AuthConfig = {
  userEntity: string
  methods: AuthMethods
  externalAuthEntity?: string
  onAuthFailedRedirectTo: string
  onAuthSucceededRedirectTo?: string
  onBeforeSignup?: ExtImport
  onAfterSignup?: ExtImport
  onBeforeOAuthRedirect?: ExtImport
  onBeforeLogin?: ExtImport
  onAfterLogin?: ExtImport
}

export type AuthMethods = {
  usernameAndPassword?: UsernameAndPasswordConfig
  discord?: ExternalAuthConfig
  google?: ExternalAuthConfig
  gitHub?: ExternalAuthConfig
  keycloak?: ExternalAuthConfig
  email?: EmailAuthConfig
}

export type UsernameAndPasswordConfig = {
  userSignupFields?: ExtImport
}

export type ExternalAuthConfig = {
  configFn?: ExtImport
  userSignupFields?: ExtImport
}

export type EmailAuthConfig = {
  userSignupFields?: ExtImport
  fromField: EmailFromField
  emailVerification: EmailVerificationConfig
  passwordReset: PasswordResetConfig
}

export type EmailSender = {
  provider: EmailProvider
  defaultFrom?: EmailFromField
}

// TODO: duplication
export type EmailProvider = 'SMTP' | 'SendGrid' | 'Mailgun' | 'Dummy'

export type EmailFromField = {
  name?: string
  email: string
}

export type EmailVerificationConfig = {
  getEmailContentFn?: ExtImport
  clientRoute: string
}

export type PasswordResetConfig = {
  getEmailContentFn?: ExtImport
  clientRoute: string
}

// TODO: Treat user-facing config objects and spec objects differently
export type UserSpec = {
  app: { name: string; config: AppConfig }
  // TODO: handle this one too
  actions: Map<string, ActionConfig>
  apiNamespaces: Map<string, ApiNamespaceConfig>
  apis: Map<string, ApiConfig>
  auth?: AuthConfig
  client?: ClientConfig
  cruds: Map<string, Crud>
  db?: DbConfig
  emailSender?: EmailSenderConfig
  jobs: Map<string, JobConfig>
  pages: Map<string, PageConfig>
  queries: Map<string, QueryConfig>
  routes: Map<string, RouteConfig>
  server?: ServerConfig
  websocket?: WebsocketConfig
}

export class App {
  private spec: UserSpec

  // TODO: I need a secret method to query the spec, let's keep it public for now
  getSpec() {
    return this.spec
  }

  constructor(name: string, config: AppConfig) {
    this.spec = {
      app: { name, config: config },
      actions: new Map(),
      apiNamespaces: new Map(),
      apis: new Map(),
      auth: undefined,
      client: undefined,
      cruds: new Map(),
      db: undefined,
      emailSender: undefined,
      jobs: new Map(),
      pages: new Map(),
      queries: new Map(),
      routes: new Map(),
      server: undefined,
      websocket: undefined,
    }
  }

  // TODO: Enforce that all methods are covered in compile time

  action(this: App, name: string, config: ActionConfig): void {
    this.spec.actions.set(name, config)
  }

  apiNamespace(this: App, name: string, config: ApiNamespaceConfig): void {
    this.spec.apiNamespaces.set(name, config)
  }

  api(this: App, name: string, config: ApiConfig): void {
    this.spec.apis.set(name, config)
  }

  auth(this: App, config: AuthConfig): void {
    this.spec.auth = config
  }

  client(this: App, config: ClientConfig): void {
    this.spec.client = config
  }

  crud(this: App, name: string, config: Crud): void {
    this.spec.cruds.set(name, config)
  }

  db(this: App, config: DbConfig): void {
    this.spec.db = config
  }

  emailSender(this: App, config: EmailSenderConfig): void {
    this.spec.emailSender = config
  }

  job(this: App, name: string, config: JobConfig): void {
    this.spec.jobs.set(name, config)
  }

  page(this: App, name: string, config: PageConfig): PageName {
    this.spec.pages.set(name, config)
    // NOTE: return a string or return a page object?
    return name as PageName
  }

  query(this: App, name: string, config: QueryConfig): void {
    this.spec.queries.set(name, config)
  }

  route(this: App, name: string, config: RouteConfig): void {
    this.spec.routes.set(name, config)
  }

  // TODO: Learn more about typing 'this'
  server(this: App, config: ServerConfig): void {
    this.spec.server = config
  }

  // NOTE: I changed this to lowercase because it's more standard
  websocket(this: App, config: WebsocketConfig) {
    this.spec.websocket = config
  }
}
