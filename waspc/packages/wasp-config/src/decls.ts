// TODO: This whole file is not DRY, we have the AppSpec specified in two
// diffeernent places: Haskell code and hare.

export type Decl =
  | { declType: 'page'; declName: string; declValue: Page }
  | { declType: 'route'; declName: string; declValue: Route }
  | { declType: 'query'; declName: string; declValue: Query }
  | { declType: 'action'; declName: string; declValue: Action }
  | { declType: 'app'; declName: string; declValue: App }
  | { declType: 'entity'; declName: string; declValue: Entity }
  | { declType: 'entity'; declName: string; declValue: Entity }
  | { declType: 'job'; declName: string; declValue: Job }
  | { declType: 'api'; declName: string; declValue: Api }
  | { declType: 'apiNamespace'; declName: string; declValue: ApiNamespace }
  | { declType: 'crud'; declName: string; declValue: Crud }

type Page = {
  component: ExtImport
  authRequired?: boolean
}

type Route = {
  path: string
  to: Ref<'page'>
}

type Action = {
  fn: ExtImport
  entities?: Ref<'entity'>[]
  auth?: boolean
}

type Query = {
  fn: ExtImport
  entities?: Ref<'entity'>[]
  auth?: boolean
}

type Job = {
  executor: JobExecutor
  perofrm: {
    fn: ExtImport
    executorOptions?: ExecutorOptions
  }
  schedule?: {
    cron: string
    args?: object
    executorOption?: ExecutorOptions
  }
  entities?: Ref<'entity'>[]
}

type Api = {
  fn: ExtImport
  middlewareConfigFn?: ExtImport
  entities?: Ref<'entity'>[]
  httpRoute: [[HttpMethod, string]]
  auth?: boolean
}

type ApiNamespace = {
  middlewareConfigFn: ExtImport
  path: String
}

type Crud = {
  entity: Ref<'entity'>
  operations: CrudOperations
}

type Entity = {}

type App = {
  wasp: Wasp
  title: string
  head?: string[]
  auth?: Auth
  server?: Server
  client?: Client
  db?: Db
  emailSender?: EmailSender
  webSocket?: WebSocket
}

type ExtImport = {
  kind: 'named' | 'default'
  name: string
  path: `@src/${string}`
}

type JobExecutor = 'PgBoss'

type ExecutorOptions = {
  pgBoss?: object
}

type HttpMethod = 'ALL' | 'GET' | 'POST' | 'PUT' | 'DELETE'

type CrudOperations = {
  get?: CrudOperationOptions
  getAll?: CrudOperationOptions
  create?: CrudOperationOptions
  update?: CrudOperationOptions
  delete?: CrudOperationOptions
}

type CrudOperationOptions = {
  isPublic?: boolean
  overrideFn?: ExtImport
}

type Wasp = {
  version: string
}

type Auth = {
  userEntity: Ref<'entity'>
  externalAuthEntity?: Ref<'entity'>
  methods: AuthMethods
  onAuthFailedRedirectTo: string
  onAuthSucceededRedirectTo?: string
  onBeforeSignup?: ExtImport
  onAfterSignup?: ExtImport
  onBeforeOAuthRedirect?: ExtImport
  onBeforeLogin?: ExtImport
  onAfterLogin?: ExtImport
}

type AuthMethods = {
  usernameAndPassword?: UsernameAndPasswordConfig
  discord?: ExternalAuthConfig
  google?: ExternalAuthConfig
  gitHub?: ExternalAuthConfig
  keycloak?: ExternalAuthConfig
  email?: EmailAuthConfig
}

type UsernameAndPasswordConfig = {
  userSignupFields?: ExtImport
}

type ExternalAuthConfig = {
  configFn?: ExtImport
  userSignupFields?: ExtImport
}

type EmailAuthConfig = {
  userSignupFields?: ExtImport
  fromField: EmailFromField
  emailVerification: EmailVerificationConfig
  passwordReset: PasswordResetConfig
}

type EmailSender = {
  provider: EmailProvider
  defaultFrom?: EmailFromField
}

// TODO: duplication
type EmailProvider = 'SMTP' | 'SendGrid' | 'Mailgun' | 'Dummy'

type EmailFromField = {
  name?: string
  email: string
}

type EmailVerificationConfig = {
  getEmailContentFn?: ExtImport
  clientRoute: Ref<'route'>
}

type PasswordResetConfig = {
  getEmailContentFn?: ExtImport
  clientRoute: Ref<'route'>
}

type Ref<T extends Decl['declType']> = T & {
  kind: 'validated'
}

type Server = {
  setupFn?: ExtImport
  middlewareConfigFn?: ExtImport
}

type Client = {
  setupFn?: ExtImport
  rootComponent?: ExtImport
  baseDir?: `/${string}`
}

type Db = {
  seeds?: ExtImport[]
}

type WebSocket = {
  fn: ExtImport
  autoConnect?: boolean
}
