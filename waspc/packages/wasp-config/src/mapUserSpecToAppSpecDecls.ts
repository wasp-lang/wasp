/** This module maps the user-facing API to the internal representation of the app (AppSpec Decl).
 */
import * as AppSpec from './appSpec.js'
import * as User from './userApi.js'

export function mapUserSpecToAppSpecDecls(
  spec: User.UserSpec,
  entityNames: string[]
): AppSpec.Decl[] {
  const {
    app,
    actions,
    apis,
    apiNamespaces,
    auth,
    client,
    db,
    emailSender,
    jobs,
    pages,
    queries,
    routes,
    server,
    websocket,
    cruds,
  } = spec

  const pageNames = Array.from(pages.keys())
  const routeNames = Array.from(routes.keys())

  const parseEntityRef = makeRefParser('Entity', entityNames)
  const parsePageRef = makeRefParser('Page', pageNames)
  const parseRouteRef = makeRefParser('Route', routeNames)

  const pageDecls = mapToDecls(pages, 'Page', mapPage)
  const routeDecls = mapToDecls(routes, 'Route', (routeConfig) =>
    mapRoute(routeConfig, parsePageRef)
  )
  const actionDecls = mapToDecls(actions, 'Action', (actionConfig) =>
    mapOperationConfig(actionConfig, parseEntityRef)
  )
  const queryDecls = mapToDecls(queries, 'Query', (queryConfig) =>
    mapOperationConfig(queryConfig, parseEntityRef)
  )
  const apiDecls = mapToDecls(apis, 'Api', (apiConfig) =>
    mapApiConfig(apiConfig, parseEntityRef)
  )
  const jobDecls = mapToDecls(jobs, 'Job', (jobConfig) =>
    mapJob(jobConfig, parseEntityRef)
  )
  const apiNamespaceDecls = mapToDecls(
    apiNamespaces,
    'ApiNamespace',
    mapApiNamespace
  )
  const crudDecls = mapToDecls(cruds, 'Crud', (crudConfig) =>
    mapCrud(crudConfig, parseEntityRef)
  )

  const appDecl = {
    declType: 'App' as const,
    declName: app.name,
    declValue: mapApp(
      app.config,
      parseEntityRef,
      parseRouteRef,
      auth,
      server,
      client,
      db,
      emailSender,
      websocket
    ),
  }

  return makeDeclsArray({
    App: [appDecl],
    Page: pageDecls,
    Route: routeDecls,
    Action: actionDecls,
    Query: queryDecls,
    Api: apiDecls,
    Job: jobDecls,
    ApiNamespace: apiNamespaceDecls,
    Crud: crudDecls,
  })
}

function makeDeclsArray(decls: {
  [Type in AppSpec.Decl['declType']]: AppSpec.GetDeclForType<Type>[]
}): AppSpec.Decl[] {
  return Object.values(decls).flatMap((decl) => [...decl])
}

function mapToDecls<T, DeclType extends AppSpec.Decl['declType']>(
  configs: Map<string, T>,
  type: DeclType,
  configToDeclValue: (
    config: T
  ) => AppSpec.GetDeclForType<DeclType>['declValue']
) {
  return [...configs].map(([name, config]) => ({
    declType: type,
    declName: name,
    declValue: configToDeclValue(config),
  }))
}

function mapOperationConfig(
  config: User.QueryConfig,
  parseEntityRef: RefParser<'Entity'>
): AppSpec.Query
function mapOperationConfig(
  config: User.ActionConfig,
  parseEntityRef: RefParser<'Entity'>
): AppSpec.Action
function mapOperationConfig(
  config: User.ActionConfig | User.QueryConfig,
  parseEntityRef: RefParser<'Entity'>
): AppSpec.Action | AppSpec.Query {
  const { fn, entities, auth } = config
  return {
    fn: mapExtImport(fn),
    entities: entities && entities.map(parseEntityRef),
    auth,
  }
}

function mapExtImport(extImport: User.ExtImport): AppSpec.ExtImport {
  if ('import' in extImport) {
    return { kind: 'named', name: extImport.import, path: extImport.from }
  } else if ('importDefault' in extImport) {
    return {
      kind: 'default',
      name: extImport.importDefault,
      path: extImport.from,
    }
  } else {
    throw new Error(
      'Invalid ExtImport: neither `import` nor `importDefault` is defined'
    )
  }
}

function mapHttpRoute(httpRoute: User.HttpRoute): AppSpec.HttpRoute {
  return [httpRoute.method, httpRoute.route]
}

function mapApiConfig(
  config: User.ApiConfig,
  parseEntityRef: RefParser<'Entity'>
): AppSpec.Api {
  const { fn, middlewareConfigFn, entities, httpRoute, auth } = config
  return {
    fn: mapExtImport(fn),
    middlewareConfigFn: middlewareConfigFn && mapExtImport(middlewareConfigFn),
    entities: entities && entities.map(parseEntityRef),
    httpRoute: mapHttpRoute(httpRoute),
    auth,
  }
}

function mapApiNamespace(
  config: User.ApiNamespaceConfig
): AppSpec.ApiNamespace {
  const { middlewareConfigFn, path } = config
  return {
    middlewareConfigFn: mapExtImport(middlewareConfigFn),
    path,
  }
}

export function mapApp(
  app: User.AppConfig,
  // TODO: Make this better, optional props are problematic so I have to pass the parsers first
  parseEntityRef: RefParser<'Entity'>,
  parseRouteRef: RefParser<'Route'>,
  auth?: User.AuthConfig,
  server?: User.ServerConfig,
  client?: User.ClientConfig,
  db?: User.DbConfig,
  emailSender?: User.EmailSenderConfig,
  webSocket?: User.WebsocketConfig
): AppSpec.App {
  const { title, wasp, head } = app
  return {
    wasp,
    title,
    head,
    auth: auth && mapAuth(auth, parseEntityRef, parseRouteRef),
    server: server && mapServer(server),
    client: client && mapClient(client),
    webSocket: webSocket && mapWebSocket(webSocket),
    db: db && mapDb(db),
    emailSender: emailSender && mapEmailSender(emailSender),
  }
}

function mapAuth(
  auth: User.AuthConfig,
  parseEntityRef: RefParser<'Entity'>,
  parseRouteRef: RefParser<'Route'>
): AppSpec.Auth {
  const {
    userEntity,
    externalAuthEntity,
    methods,
    onAuthFailedRedirectTo,
    onAuthSucceededRedirectTo,
    onBeforeSignup,
    onAfterSignup,
    onBeforeOAuthRedirect,
    onBeforeLogin,
    onAfterLogin,
  } = auth
  return {
    userEntity: parseEntityRef(userEntity),
    // TODO: Abstract away this pattern
    externalAuthEntity:
      externalAuthEntity === undefined
        ? undefined
        : parseEntityRef(externalAuthEntity),
    methods: mapAuthMethods(methods, parseRouteRef),
    onAuthFailedRedirectTo,
    onAuthSucceededRedirectTo,
    onBeforeSignup: onBeforeSignup && mapExtImport(onBeforeSignup),
    onAfterSignup: onAfterSignup && mapExtImport(onAfterSignup),
    onBeforeOAuthRedirect:
      onBeforeOAuthRedirect && mapExtImport(onBeforeOAuthRedirect),
    onBeforeLogin: onBeforeLogin && mapExtImport(onBeforeLogin),
    onAfterLogin: onAfterLogin && mapExtImport(onAfterLogin),
  }
}

function mapAuthMethods(
  methods: User.AuthMethods,
  parseRouteRef: RefParser<'Route'>
): AppSpec.AuthMethods {
  // TODO: check keyof danger, effective ts
  const { usernameAndPassword, discord, google, gitHub, keycloak, email } =
    methods
  return {
    usernameAndPassword:
      usernameAndPassword && mapUsernameAndPassword(usernameAndPassword),
    discord: discord && mapExternalAuth(discord),
    google: google && mapExternalAuth(google),
    gitHub: gitHub && mapExternalAuth(gitHub),
    keycloak: keycloak && mapExternalAuth(keycloak),
    email: email && mapEmailAuth(email, parseRouteRef),
  }
}

function mapUsernameAndPassword(
  usernameAndPassword: User.UsernameAndPasswordConfig
): AppSpec.UsernameAndPasswordConfig {
  const { userSignupFields } = usernameAndPassword
  return {
    userSignupFields: userSignupFields && mapExtImport(userSignupFields),
  }
}

function mapExternalAuth(
  externalAuth: User.ExternalAuthConfig
): AppSpec.ExternalAuthConfig {
  const { configFn, userSignupFields } = externalAuth
  return {
    configFn: configFn && mapExtImport(configFn),
    userSignupFields: userSignupFields && mapExtImport(userSignupFields),
  }
}

function mapEmailAuth(
  emailConfig: User.EmailAuthConfig,
  parseRouteRef: RefParser<'Route'>
): AppSpec.EmailAuthConfig {
  const {
    userSignupFields,
    fromField: { name, email },
    emailVerification,
    passwordReset,
  } = emailConfig
  return {
    userSignupFields: userSignupFields && mapExtImport(userSignupFields),
    fromField: {
      name,
      email,
    },
    emailVerification: mapEmailVerification(emailVerification, parseRouteRef),
    passwordReset: mapPasswordReset(passwordReset, parseRouteRef),
  }
}

function mapEmailVerification(
  emailVerification: User.EmailVerificationConfig,
  parseRouteRef: RefParser<'Route'>
): AppSpec.EmailVerificationConfig {
  const { getEmailContentFn, clientRoute } = emailVerification
  return {
    getEmailContentFn: getEmailContentFn && mapExtImport(getEmailContentFn),
    clientRoute: parseRouteRef(clientRoute),
  }
}

function mapPasswordReset(
  passwordReset: User.PasswordResetConfig,
  parseRouteRef: RefParser<'Route'>
): AppSpec.PasswordResetConfig {
  const { getEmailContentFn, clientRoute } = passwordReset
  return {
    getEmailContentFn: getEmailContentFn && mapExtImport(getEmailContentFn),
    clientRoute: parseRouteRef(clientRoute),
  }
}

function mapDb(db: User.DbConfig): AppSpec.Db {
  const { seeds } = db
  return {
    seeds: seeds && seeds.map(mapExtImport),
  }
}

function mapEmailSender(
  emailSender: User.EmailSenderConfig
): AppSpec.EmailSender {
  const { provider, defaultFrom } = emailSender
  return {
    provider,
    defaultFrom: defaultFrom && {
      name: defaultFrom.name,
      email: defaultFrom.email,
    },
  }
}

function mapServer(server: User.ServerConfig): AppSpec.Server {
  const { setupFn, middlewareConfigFn, envValidationSchema } = server
  return {
    setupFn: setupFn && mapExtImport(setupFn),
    middlewareConfigFn: middlewareConfigFn && mapExtImport(middlewareConfigFn),
    envValidationSchema:
      envValidationSchema && mapExtImport(envValidationSchema),
  }
}

function mapClient(client: User.ClientConfig): AppSpec.Client {
  const { setupFn, rootComponent, baseDir, envValidationSchema } = client
  return {
    setupFn: setupFn && mapExtImport(setupFn),
    rootComponent: rootComponent && mapExtImport(rootComponent),
    baseDir,
    envValidationSchema:
      envValidationSchema && mapExtImport(envValidationSchema),
  }
}

function mapWebSocket(websocket: User.WebsocketConfig): AppSpec.WebSocket {
  const { fn, autoConnect } = websocket
  return {
    fn: mapExtImport(fn),
    autoConnect,
  }
}

function mapJob(
  job: User.JobConfig,
  parseEntityRef: RefParser<'Entity'>
): AppSpec.Job {
  const { executor, perform, schedule, entities } = job
  return {
    executor,
    perform: mapPerform(perform),
    schedule: schedule && mapSchedule(schedule),
    entities: entities && entities.map(parseEntityRef),
  }
}

function mapSchedule(schedule: User.ScheduleConfig): AppSpec.Schedule {
  const { cron, args, executorOptions } = schedule
  return {
    cron,
    args,
    executorOptions,
  }
}
function mapPerform(perform: User.Perform): AppSpec.Perform {
  const { fn, executorOptions } = perform
  return {
    fn: mapExtImport(fn),
    executorOptions,
  }
}

function mapRoute(
  route: User.RouteConfig,
  parsePageRef: RefParser<'Page'>
): AppSpec.Route {
  const { path, to } = route
  return {
    path,
    to: parsePageRef(to),
  }
}
function mapCrud(
  crudConfig: User.Crud,
  parseEntityRef: RefParser<'Entity'>
): AppSpec.Crud {
  const { entity, operations } = crudConfig
  return {
    entity: parseEntityRef(entity),
    operations: mapCrudOperations(operations),
  }
}

function mapCrudOperations(
  operations: User.CrudOperations
): AppSpec.CrudOperations {
  const { get, getAll, create, update, delete: del } = operations
  // TODO: Do this for all keys
  return {
    get: get && mapCrudOperationOptions(get),
    getAll: getAll && mapCrudOperationOptions(getAll),
    create: create && mapCrudOperationOptions(create),
    update: update && mapCrudOperationOptions(update),
    delete: del && mapCrudOperationOptions(del),
  }
}

function mapCrudOperationOptions(
  options: User.CrudOperationOptions
): AppSpec.CrudOperationOptions {
  const { isPublic, overrideFn } = options
  return {
    isPublic,
    overrideFn: overrideFn && mapExtImport(overrideFn),
  }
}

function mapPage(pageConfig: User.PageConfig): AppSpec.Page {
  const { component, authRequired } = pageConfig
  return {
    component: mapExtImport(component),
    authRequired,
  }
}

type RefParser<T extends AppSpec.DeclType> = (
  potentialReferences: string
) => AppSpec.Ref<T>

export function makeRefParser<T extends AppSpec.DeclType>(
  declType: T,
  declNames: string[]
): RefParser<T> {
  return function parseRef(potentialRef: string): AppSpec.Ref<T> {
    if (!declNames.includes(potentialRef)) {
      throw new Error(`Invalid ${declType} reference: ${potentialRef}`)
    }
    return {
      name: potentialRef,
      declType,
    } as AppSpec.Ref<T>
  }
}
