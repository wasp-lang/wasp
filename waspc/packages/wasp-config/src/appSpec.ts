// TODO: This whole file is not DRY, we have the AppSpec specified in two
// diffeernent places: Haskell code and hare.

export type Decl =
  | { declType: 'App'; declName: string; declValue: App }
  | { declType: 'Page'; declName: string; declValue: Page }
  | { declType: 'Route'; declName: string; declValue: Route }
  | { declType: 'Query'; declName: string; declValue: Query }
  | { declType: 'Action'; declName: string; declValue: Action }
  | { declType: 'App'; declName: string; declValue: App }
  | { declType: 'Entity'; declName: string; declValue: Entity }
  | { declType: 'Job'; declName: string; declValue: Job }
  | { declType: 'Api'; declName: string; declValue: Api }
  | { declType: 'ApiNamespace'; declName: string; declValue: ApiNamespace }
  | { declType: 'Crud'; declName: string; declValue: Crud }

export type TypeChecker<T extends Decl['declType']> = (
  potentialReferences: string
) => Ref<T>

export function makeTypeChecker<T extends Decl['declType']>(
  declType: T,
  declNames: string[]
): TypeChecker<T> {
  return (potentialReference: string): Ref<T> => {
    console.log({ potentialReference, declNames })
    if (!declNames.includes(potentialReference)) {
      console.log(`Invalid ${declType} reference: ${potentialReference}`)
      throw new Error(`Invalid ${declType} reference: ${potentialReference}`)
    }
    return {
      name: potentialReference,
      declType,
    } as Ref<T>
  }
}

export type Page = {
  component: ExtImport
  authRequired?: boolean
}

export type Route = {
  path: string
  to: Ref<'Page'>
}

export type Action = {
  fn: ExtImport
  entities?: Ref<'Entity'>[]
  auth?: boolean
}

export type Query = {
  fn: ExtImport
  entities?: Ref<'Entity'>[]
  auth?: boolean
}

export type Job = {
  executor: JobExecutor
  perform: Perform
  schedule?: Schedule
  entities?: Ref<'Entity'>[]
}
export type Schedule = {
  cron: string
  args?: object
  executorOptions?: ExecutorOptions
}

export type Perform = {
  fn: ExtImport
  executorOptions?: ExecutorOptions
}

export type Api = {
  fn: ExtImport
  middlewareConfigFn?: ExtImport
  entities?: Ref<'Entity'>[]
  httpRoute: HttpRoute[]
  auth?: boolean
}

export type ApiNamespace = {
  middlewareConfigFn: ExtImport
  path: string
}

export type Crud = {
  entity: Ref<'Entity'>
  operations: CrudOperations
}

export type App = {
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

export type ExtImport = {
  kind: 'named' | 'default'
  name: string
  path: `@src/${string}`
}

export type JobExecutor = 'PgBoss'

export type ExecutorOptions = {
  pgBoss?: object
}

export type HttpMethod = 'ALL' | 'GET' | 'POST' | 'PUT' | 'DELETE'

export type HttpRoute = [HttpMethod, string]

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

export type Wasp = {
  // TODO: Check semver in export type system?
  version: string
}

export type Auth = {
  userEntity: Ref<'Entity'>
  externalAuthEntity?: Ref<'Entity'>
  methods: AuthMethods
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
  clientRoute: Ref<'Route'>
}

export type PasswordResetConfig = {
  getEmailContentFn?: ExtImport
  clientRoute: Ref<'Route'>
}

export type Ref<T extends Decl['declType']> = {
  name: string
  declType: T
}

export type Server = {
  setupFn?: ExtImport
  middlewareConfigFn?: ExtImport
}

export type Client = {
  setupFn?: ExtImport
  rootComponent?: ExtImport
  baseDir?: `/${string}`
}

export type Db = {
  seeds?: ExtImport[]
}

export type WebSocket = {
  fn: ExtImport
  autoConnect?: boolean
}
