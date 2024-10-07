import * as AppSpec from './appSpec.js'
import * as User from './userApi.js'

export function mapUserSpecToDecls(
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
  } = spec

  const pageNames = Array.from(pages.keys())
  const routeNames = Array.from(routes.keys())
  const parseEntityRef = AppSpec.makeRefParser('Entity', entityNames)
  const parsePageRef = AppSpec.makeRefParser('Page', pageNames)
  const parseRouteRef = AppSpec.makeRefParser('Route', routeNames)

  // TODO: Try to build the entire object at once
  const decls: AppSpec.Decl[] = []

  // TODO: Find a way to make sure you've covered everything in compile time
  for (const [pageName, pageConfig] of pages.entries()) {
    decls.push({
      declType: 'Page',
      declName: pageName,
      declValue: mapPage(pageConfig),
    })
  }

  for (const [routeName, routeConfig] of routes.entries()) {
    decls.push({
      declType: 'Route',
      declName: routeName,
      declValue: mapRoute(routeConfig, parsePageRef),
    })
  }

  decls.push({
    declType: 'App',
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
  })

  for (const [actionName, actionConfig] of actions.entries()) {
    decls.push({
      declType: 'Action',
      declName: actionName,
      declValue: mapOperationConfig(actionConfig, parseEntityRef),
    })
  }

  for (const [queryName, queryConfig] of queries.entries()) {
    decls.push({
      declType: 'Query',
      declName: queryName,
      declValue: mapOperationConfig(queryConfig, parseEntityRef),
    })
  }

  for (const [apiName, apiConfig] of apis.entries()) {
    decls.push({
      declType: 'Api',
      declName: apiName,
      declValue: mapApiConfig(apiConfig, parseEntityRef),
    })
  }

  for (const [jobName, jobConfig] of jobs.entries()) {
    decls.push({
      declType: 'Job',
      declName: jobName,
      declValue: mapJob(jobConfig, parseEntityRef),
    })
  }

  for (const [
    apiNamespaceName,
    apiNamespaceConfig,
  ] of apiNamespaces.entries()) {
    decls.push({
      declType: 'ApiNamespace',
      declName: apiNamespaceName,
      declValue: mapApiNamespace(apiNamespaceConfig),
    })
  }

  for (const [crudName, crudConfig] of spec.cruds.entries()) {
    decls.push({
      declType: 'Crud',
      declName: crudName,
      declValue: mapCrud(crudConfig, parseEntityRef),
    })
  }

  return decls
}

function mapOperationConfig(
  config: User.QueryConfig,
  parseEntityRef: AppSpec.RefParser<'Entity'>
): AppSpec.Query
function mapOperationConfig(
  config: User.ActionConfig,
  parseEntityRef: AppSpec.RefParser<'Entity'>
): AppSpec.Action
function mapOperationConfig(
  config: User.ActionConfig | User.QueryConfig,
  parseEntityRef: AppSpec.RefParser<'Entity'>
): AppSpec.Action | AppSpec.Query {
  // TODO: How to make sure I've destructured everything?
  const { fn, entities, auth } = config
  return {
    fn: mapExtImport(fn),
    entities: entities.map((e) => parseEntityRef(e)),
    auth: auth,
  }
}

function mapExtImport(extImport: User.ExtImport): AppSpec.ExtImport {
  const { import: name, from: path } = extImport
  return { kind: 'named', name, path }
}

function mapApiConfig(
  config: User.ApiConfig,
  parseEntityRef: AppSpec.RefParser<'Entity'>
): AppSpec.Api {
  const { fn, middlewareConfigFn, entities, httpRoute, auth } = config
  return {
    fn: mapExtImport(fn),
    middlewareConfigFn: middlewareConfigFn && mapExtImport(middlewareConfigFn),
    entities: entities.map((e) => parseEntityRef(e)),
    httpRoute: httpRoute,
    auth: auth,
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

function mapApp(
  app: User.AppConfig,
  // TODO: Make this better, optional props are problematic so I have to pass the parsers first
  parseEntityRef: AppSpec.RefParser<'Entity'>,
  parseRouteRef: AppSpec.RefParser<'Route'>,
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
  parseEntityRef: AppSpec.RefParser<'Entity'>,
  parseRouteRef: AppSpec.RefParser<'Route'>
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
    // TODO: Abstract pattern
    ...(externalAuthEntity && {
      externalAuthEntity: parseEntityRef(externalAuthEntity),
    }),
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
  parseRouteRef: AppSpec.RefParser<'Route'>
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

export function mapExternalAuth(
  externalAuth: User.ExternalAuthConfig
): AppSpec.ExternalAuthConfig {
  const { configFn, userSignupFields } = externalAuth
  return {
    configFn: configFn && mapExtImport(configFn),
    userSignupFields: userSignupFields && mapExtImport(userSignupFields),
  }
}

function mapEmailAuth(
  email: User.EmailAuthConfig,
  parseRouteRef: AppSpec.RefParser<'Route'>
): AppSpec.EmailAuthConfig {
  const { userSignupFields, fromField, emailVerification, passwordReset } =
    email
  return {
    userSignupFields: userSignupFields && mapExtImport(userSignupFields),
    fromField,
    emailVerification: mapEmailVerification(emailVerification, parseRouteRef),
    passwordReset: mapPasswordReset(passwordReset, parseRouteRef),
  }
}

function mapEmailVerification(
  emailVerification: User.EmailVerificationConfig,
  parseRouteRef: AppSpec.RefParser<'Route'>
): AppSpec.EmailVerificationConfig {
  const { getEmailContentFn, clientRoute } = emailVerification
  return {
    getEmailContentFn: getEmailContentFn && mapExtImport(getEmailContentFn),
    clientRoute: parseRouteRef(clientRoute),
  }
}

export function mapPasswordReset(
  passwordReset: User.PasswordResetConfig,
  parseRouteRef: AppSpec.RefParser<'Route'>
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
  return emailSender
}

function mapServer(server: User.ServerConfig): AppSpec.Server {
  const { setupFn, middlewareConfigFn } = server
  return {
    setupFn: mapExtImport(setupFn),
    middlewareConfigFn: mapExtImport(middlewareConfigFn),
  }
}

function mapClient(client: User.ClientConfig): AppSpec.Client {
  const { setupFn, rootComponent } = client
  return {
    setupFn: mapExtImport(setupFn),
    rootComponent: mapExtImport(rootComponent),
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
  parseEntityRef: AppSpec.RefParser<'Entity'>
): AppSpec.Job {
  const { executor, perform, schedule, entities } = job
  return {
    executor: executor,
    perform: mapPerform(perform),
    schedule: schedule && mapSchedule(schedule),
    entities: entities.map((e) => parseEntityRef(e)),
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
    ...(executorOptions && { executorOptions }),
  }
}

function mapRoute(
  route: User.RouteConfig,
  parsePageRef: AppSpec.RefParser<'Page'>
): AppSpec.Route {
  const { path, to } = route
  return {
    path,
    to: parsePageRef(to),
  }
}
function mapCrud(
  crudConfig: User.Crud,
  parseEntityRef: AppSpec.RefParser<'Entity'>
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
