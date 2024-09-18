export type WaspConfig = {
  // TODO: Check semver in type system?
  version: string
}

export type AppConfig = {
  title: string
  wasp: WaspConfig
}

export type ExtImport = {
  import: string
  from: string
}

export type ServerConfig = {
  setupFn: ExtImport
  middlewareConfigFn: ExtImport
}

export type PageConfig = {
  name: string
  component: ExtImport
}

export type WebsocketConfig = {
  fn: ExtImport
  autoConnect: boolean
}

export type ClientConfig = {
  rootComponent: ExtImport
  setupFn: ExtImport
}

export type DatabaseConfig = {
  // TODO: Include all properties
  system: 'PostgreSQL' | 'SQLite'
  seeds?: ExtImport[]
}

export type RouteConfig = {
  name: string
  path: string
  to: PageName
}

// Do the trick with brands and symbols
const pageBrand = Symbol('Page')

type PageName = string & { _brand: typeof pageBrand }

// Current declarations:
// Action
// Api
// ApiNamespace
// App
// Crud
// ** Entity
// Job
// Page
// ** Perform - greska
// Query
// Route
// ** Schedule - greska
// Wasp

// New declarations:
// Auth
// Client
// Database
// Server
// Websocket
//

// TODO: I have to pull this out of the schema.prisma somehow
type Entity = string

type ActionConfig = {
  name: string
  fn: ExtImport
  entities: Entity[]
  auth: boolean | null
}

type ApiNamespace = {
  middlewareConfigFn: ExtImport
  path: string
}

type Api = {
  fn: ExtImport
  middlewareConfigFn: ExtImport | null
  entities: Entity[]
  httpRoute: ['ALL' | 'GET' | 'POST' | 'PUT' | 'DELETE', string]
  auth: boolean | null
}

type Job = {
  executor: 'PgBoss'
  perform: Perform
  schedule: Schedule | null
  entities: Entity[]
}

type Perform = {
  fn: ExtImport
  executorOptions: ExecutorOptions | null
}

type Schedule = {
  cron: string
  args: JSON | null
  executorOptions: ExecutorOptions | null
}

type ExecutorOptions = {
  // TODO: Type this better
  pgBoss: object
}

type QueryConfig = {
  name: string
  fn: ExtImport
  entities: Entity[]
  auth: boolean
}

type Auth = {}

// TODO: Treat user-facing config objects and spec objects differently
export type Spec = {
  app: AppConfig
  actions: ActionConfig[]
  apis: Api[]
  appNamespaces: ApiNamespace[]
  // TODO: handle this one too
  // cruds: Crud[]
  jobs: Job[]
  pages: PageConfig[]
  queries: QueryConfig[]
  routes: RouteConfig[]
  auth: Auth | null
  client: ClientConfig | null
  database: DatabaseConfig | null
  server: ServerConfig | null
  websocket: WebsocketConfig | null
}

const INITIAL_SPEC: Omit<Spec, 'app'> = {
  actions: [],
  apis: [],
  appNamespaces: [],
  jobs: [],
  pages: [],
  queries: [],
  routes: [],
  auth: null,
  client: null,
  database: null,
  server: null,
  websocket: null,
}

export class App {
  private spec: Spec

  // TODO: I need a secret method to query the spec, let's keep it public for now
  getSpec() {
    return this.spec
  }

  constructor(appConfig: AppConfig) {
    this.spec = { app: appConfig, ...INITIAL_SPEC }
  }

  // NOTE: I changed this to lowercase because it's more standard
  websocket(this: App, config: WebsocketConfig) {
    this.spec.websocket = config
  }

  // TODO: Learn more about typing 'this'
  server(this: App, config: ServerConfig): void {
    this.spec.server = config
  }

  client(this: App, config: ClientConfig): void {
    this.spec.client = config
  }

  database(this: App, config: DatabaseConfig): void {
    this.spec.database = config
  }

  page(this: App, config: PageConfig): PageName {
    this.spec.pages.push({ ...config })
    // NOTE: return a string or return a page object?
    return config.name as PageName
  }

  // TODO: Return to this and try some more
  // pageAlternative<CurrentApp, PageName extends string>(
  //   this: CurrentApp,
  //   name: PageName,
  //   config: PageConfig
  // ): asserts this is {
  //   test: boolean
  //   routeAlternative(
  //     this: CurrentApp,
  //     name: string,
  //     config: RouteConfig<ExistingPages | PageName>
  //   ): void
  // } {}
  //
  // routeAlternative<CurrentApp, PageName extends string>(
  //   this: CurrentApp,
  //   name: string,
  //   config: RouteConfig<ExistingPages>
  // ): void {}

  // pageAlternative<CurrentApp, PageName extends string>(this: App, name: PageName, config: PageConfig): asserts this is { pages: [...CurrentApp['pages'], PageName] } {
  //   this.spec.pages.push({ name, ...config })
  // }

  route(this: App, config: RouteConfig): void {
    this.spec.routes.push({ ...config })
  }

  action(this: App, config: ActionConfig): void {
    this.spec.actions.push({ ...config })
  }

  query(this: App, config: QueryConfig): void {
    this.spec.queries.push({ ...config })
  }
}
