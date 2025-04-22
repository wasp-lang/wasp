import { describe, expect, test } from 'vitest'
import * as AppSpec from '../appSpec.js'
import { getUserSpec } from '../mapUserSpecToAppSepcJson.js'
import {
  makeRefParser,
  mapApiConfig,
  mapApiNamespace,
  mapApp,
  mapAuth,
  mapAuthMethods,
  mapClient,
  mapCrud,
  mapCrudOperationOptions,
  mapCrudOperations,
  mapDb,
  mapEmailAuth,
  mapEmailSender,
  mapEmailVerification,
  mapExternalAuth,
  mapExtImport,
  mapHttpRoute,
  mapJob,
  mapOperationConfig,
  mapPage,
  mapPasswordReset,
  mapPerform,
  mapRoute,
  mapSchedule,
  mapServer,
  mapUsernameAndPassword,
  mapUserSpecToAppSpecDecls,
  mapWebSocket,
} from '../mapUserSpecToAppSpecDecls.js'
import * as UserSpec from '../userApi.js'
import {
  ACTION,
  API,
  API_NAMESPACE,
  APP,
  AUTH,
  CLIENT,
  CRUD,
  DB,
  EMAIL,
  ENTITIES,
  JOB,
  PAGE,
  QUERY,
  ROUTE,
  SERVER,
  WEBSOCKET,
} from './testFixtures.js'

describe('mapUserSpecToAppSpecDecls', () => {
  // This test is debilitaterly not using mapping functions but raw values.
  test('correctly transforms a complete app configuration', () => {
    const app = new UserSpec.App(APP.NAME, APP.CONFIG)
    app.action(ACTION.NAME, ACTION.CONFIG)
    app.apiNamespace(API_NAMESPACE.NAME, API_NAMESPACE.CONFIG)
    app.api(API.NAME, API.CONFIG)
    app.auth(AUTH.CONFIG)
    app.client(CLIENT.CONFIG)
    app.crud(CRUD.NAME, CRUD.CONFIG)
    app.db(DB.CONFIG)
    app.emailSender(EMAIL.CONFIG)
    app.job(JOB.NAME, JOB.CONFIG)
    app.page(PAGE.LOGIN.NAME, PAGE.LOGIN.CONFIG)
    app.page(PAGE.EMAIL_VERIFICATION.NAME, PAGE.EMAIL_VERIFICATION.CONFIG)
    app.page(PAGE.PASSWORD_RESET.NAME, PAGE.PASSWORD_RESET.CONFIG)
    app.route(ROUTE.LOGIN.NAME, ROUTE.LOGIN.CONFIG)
    app.route(ROUTE.EMAIL_VERIFICATION.NAME, ROUTE.EMAIL_VERIFICATION.CONFIG)
    app.route(ROUTE.PASSWORD_RESET.NAME, ROUTE.PASSWORD_RESET.CONFIG)
    app.query(QUERY.NAME, QUERY.CONFIG)
    app.server(SERVER.CONFIG)
    app.webSocket(WEBSOCKET.CONFIG)

    const userSpec = getUserSpec(app)
    const result = mapUserSpecToAppSpecDecls(userSpec, ENTITIES)

    const declTypes = result.map((decl) => decl.declType)
    const declNames = result.map((decl) => decl.declName)

    // AppConfig
    expect(declTypes).toContain('App')
    expect(declNames).toContain(APP.NAME)

    const appDecl = result.find(
      (decl) => decl.declType === 'App'
    ) as AppSpec.GetDeclForType<'App'>

    expect(appDecl).toBeDefined()
    expect(appDecl.declValue.title).toBe(APP.CONFIG.title)
    expect(appDecl.declValue.wasp.version).toBe(APP.CONFIG.wasp.version)
    expect(appDecl.declValue.head).toBeDefined()
    expect(appDecl.declValue.head).toHaveLength(1)
    expect(appDecl.declValue.head?.[0]).toBe(APP.CONFIG.head?.[0])

    // AuthConfig
    const auth = appDecl.declValue.auth

    expect(auth).toBeDefined()
    expect(auth?.userEntity).toStrictEqual({
      name: AUTH.CONFIG.userEntity,
      declType: 'Entity',
    })
    expect(auth?.externalAuthEntity).toStrictEqual({
      name: AUTH.CONFIG.externalAuthEntity,
      declType: 'Entity',
    })

    // Discord
    expect(auth?.methods.discord).toBeDefined()
    expect(auth?.methods.discord?.configFn).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.discord.configFn.import,
      path: AUTH.CONFIG.methods.discord.configFn.from,
    })
    expect(auth?.methods.discord?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.discord.userSignupFields.import,
      path: AUTH.CONFIG.methods.discord.userSignupFields.from,
    })

    // GitHub
    expect(auth?.methods.gitHub).toBeDefined()
    expect(auth?.methods.gitHub?.configFn).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.gitHub.configFn.import,
      path: AUTH.CONFIG.methods.gitHub.configFn.from,
    })
    expect(auth?.methods.gitHub?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.gitHub.userSignupFields.import,
      path: AUTH.CONFIG.methods.gitHub.userSignupFields.from,
    })

    // Keycloak
    expect(auth?.methods.keycloak).toBeDefined()
    expect(auth?.methods.keycloak?.configFn).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.keycloak.configFn.import,
      path: AUTH.CONFIG.methods.keycloak.configFn.from,
    })
    expect(auth?.methods.keycloak?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.keycloak.userSignupFields.import,
      path: AUTH.CONFIG.methods.keycloak.userSignupFields.from,
    })

    // Google
    expect(auth?.methods.google).toBeDefined()
    expect(auth?.methods.google?.configFn).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.google.configFn.import,
      path: AUTH.CONFIG.methods.google.configFn.from,
    })
    expect(auth?.methods.google?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.google.userSignupFields.import,
      path: AUTH.CONFIG.methods.google.userSignupFields.from,
    })

    // Username and Password
    expect(auth?.methods.usernameAndPassword).toBeDefined()
    expect(auth?.methods.usernameAndPassword?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.usernameAndPassword.userSignupFields.import,
      path: AUTH.CONFIG.methods.usernameAndPassword.userSignupFields.from,
    })

    // Email
    expect(auth?.methods.email).toBeDefined()
    expect(auth?.methods.email?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.methods.email.userSignupFields.import,
      path: AUTH.CONFIG.methods.email.userSignupFields.from,
    })
    expect(auth?.methods.email?.fromField).toStrictEqual({
      name: AUTH.CONFIG.methods.email.fromField.name,
      email: AUTH.CONFIG.methods.email.fromField.email,
    })
    expect(auth?.methods.email?.emailVerification).toStrictEqual({
      getEmailContentFn: {
        kind: 'named',
        name: AUTH.CONFIG.methods.email.emailVerification.getEmailContentFn
          .import,
        path: AUTH.CONFIG.methods.email.emailVerification.getEmailContentFn
          .from,
      },
      clientRoute: {
        name: AUTH.CONFIG.methods.email.emailVerification.clientRoute,
        declType: 'Route',
      },
    })
    expect(auth?.methods.email?.passwordReset).toStrictEqual({
      getEmailContentFn: {
        kind: 'named',
        name: AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn.import,
        path: AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn.from,
      },
      clientRoute: {
        name: AUTH.CONFIG.methods.email.passwordReset.clientRoute,
        declType: 'Route',
      },
    })

    // Auth Hooks
    expect(auth?.onAuthFailedRedirectTo).toBe(
      AUTH.CONFIG.onAuthFailedRedirectTo
    )
    expect(auth?.onAuthSucceededRedirectTo).toBe(
      AUTH.CONFIG.onAuthSucceededRedirectTo
    )
    expect(auth?.onBeforeOAuthRedirect).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.onBeforeOAuthRedirect.import,
      path: AUTH.CONFIG.onBeforeOAuthRedirect.from,
    })
    expect(auth?.onBeforeSignup).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.onBeforeSignup.import,
      path: AUTH.CONFIG.onBeforeSignup.from,
    })
    expect(auth?.onAfterSignup).toStrictEqual({
      kind: 'named',
      name: AUTH.CONFIG.onAfterSignup.import,
      path: AUTH.CONFIG.onAfterSignup.from,
    })

    // ClientConfig
    const client = appDecl.declValue.client

    expect(client).toBeDefined()
    expect(client?.rootComponent).toStrictEqual({
      kind: 'named',
      name: CLIENT.CONFIG.rootComponent.import,
      path: CLIENT.CONFIG.rootComponent.from,
    })
    expect(client?.setupFn).toStrictEqual({
      kind: 'named',
      name: CLIENT.CONFIG.setupFn.import,
      path: CLIENT.CONFIG.setupFn.from,
    })
    expect(client?.baseDir).toBe(CLIENT.CONFIG.baseDir)
    expect(client?.envValidationSchema).toStrictEqual({
      kind: 'named',
      name: CLIENT.CONFIG.envValidationSchema.import,
      path: CLIENT.CONFIG.envValidationSchema.from,
    })

    // DbConfig
    const db = appDecl.declValue.db

    expect(db).toBeDefined()
    expect(db?.seeds).toHaveLength(1)
    expect(db?.seeds?.[0]).toStrictEqual({
      kind: 'named',
      name: DB.CONFIG.seeds[0]?.import,
      path: DB.CONFIG.seeds[0]?.from,
    })

    // EmailSenderConfig
    const emailSender = appDecl.declValue.emailSender

    expect(emailSender).toBeDefined()
    expect(emailSender?.provider).toBe(EMAIL.CONFIG.provider)
    expect(emailSender?.defaultFrom?.email).toBe(EMAIL.CONFIG.defaultFrom.email)
    expect(emailSender?.defaultFrom?.name).toBe(EMAIL.CONFIG.defaultFrom.name)

    // ServerConfig
    const server = appDecl.declValue.server

    expect(server).toBeDefined()
    expect(server?.setupFn).toStrictEqual({
      kind: 'named',
      name: SERVER.CONFIG.setupFn.import,
      path: SERVER.CONFIG.setupFn.from,
    })
    expect(server?.middlewareConfigFn).toStrictEqual({
      kind: 'named',
      name: SERVER.CONFIG.middlewareConfigFn.import,
      path: SERVER.CONFIG.middlewareConfigFn.from,
    })
    expect(server?.envValidationSchema).toStrictEqual({
      kind: 'named',
      name: SERVER.CONFIG.envValidationSchema.import,
      path: SERVER.CONFIG.envValidationSchema.from,
    })

    // WebSocketConfig
    const webSocket = appDecl.declValue.webSocket

    expect(webSocket).toBeDefined()
    expect(webSocket?.fn).toStrictEqual({
      kind: 'named',
      name: WEBSOCKET.CONFIG.fn.import,
      path: WEBSOCKET.CONFIG.fn.from,
    })

    // ActionConfig
    expect(declTypes).toContain('Action')
    expect(declNames).toContain(ACTION.NAME)

    const actionDecl = result.find(
      (decl) => decl.declType === 'Action' && decl.declName === ACTION.NAME
    ) as AppSpec.GetDeclForType<'Action'>

    expect(actionDecl).toBeDefined()
    expect(actionDecl.declValue.fn).toStrictEqual({
      kind: 'named',
      name: ACTION.CONFIG.fn.import,
      path: ACTION.CONFIG.fn.from,
    })
    expect(actionDecl.declValue.entities).toBeDefined()
    expect(actionDecl.declValue.entities).toHaveLength(1)
    expect(actionDecl.declValue.entities?.[0]).toStrictEqual({
      name: ACTION.CONFIG.entities[0],
      declType: 'Entity',
    })
    expect(actionDecl.declValue.auth).toBe(ACTION.CONFIG.auth)

    // ApiNamespaceConfig
    expect(declTypes).toContain('ApiNamespace')
    expect(declNames).toContain(API_NAMESPACE.NAME)

    const apiNamespaceDecl = result.find(
      (decl) =>
        decl.declType === 'ApiNamespace' && decl.declName === API_NAMESPACE.NAME
    ) as AppSpec.GetDeclForType<'ApiNamespace'>

    expect(apiNamespaceDecl).toBeDefined()
    expect(apiNamespaceDecl.declValue.path).toBe(API_NAMESPACE.CONFIG.path)
    expect(apiNamespaceDecl.declValue.middlewareConfigFn).toStrictEqual({
      kind: 'named',
      name: API_NAMESPACE.CONFIG.middlewareConfigFn.import,
      path: API_NAMESPACE.CONFIG.middlewareConfigFn.from,
    })

    // ApiConfig
    expect(declTypes).toContain('Api')
    expect(declNames).toContain(API.NAME)

    const apiDecl = result.find(
      (decl) => decl.declType === 'Api' && decl.declName === API.NAME
    ) as AppSpec.GetDeclForType<'Api'>

    expect(apiDecl.declValue.fn).toStrictEqual({
      kind: 'named',
      name: API.CONFIG.fn.import,
      path: API.CONFIG.fn.from,
    })
    expect(apiDecl.declValue.auth).toBe(API.CONFIG.auth)
    expect(apiDecl.declValue.httpRoute).toStrictEqual([
      API.CONFIG.httpRoute.method,
      API.CONFIG.httpRoute.route,
    ])
    expect(apiDecl.declValue.entities).toBeDefined()
    expect(apiDecl.declValue.entities).toHaveLength(1)
    expect(apiDecl.declValue.entities?.[0]).toStrictEqual({
      name: API.CONFIG.entities[0],
      declType: 'Entity',
    })
    expect(apiDecl.declValue.middlewareConfigFn).toStrictEqual({
      kind: 'named',
      name: API.CONFIG.middlewareConfigFn.import,
      path: API.CONFIG.middlewareConfigFn.from,
    })

    // CrudConfig
    expect(declTypes).toContain('Crud')
    expect(declNames).toContain(CRUD.NAME)

    const crudDecl = result.find(
      (decl) => decl.declType === 'Crud' && decl.declName === CRUD.NAME
    ) as AppSpec.GetDeclForType<'Crud'>

    expect(crudDecl).toBeDefined()
    expect(crudDecl.declValue.entity).toStrictEqual({
      name: CRUD.CONFIG.entity,
      declType: 'Entity',
    })
    expect(crudDecl.declValue.operations.get).toBeDefined()
    expect(crudDecl.declValue.operations.get?.isPublic).toBe(
      CRUD.CONFIG.operations.get.isPublic
    )
    expect(crudDecl.declValue.operations.get?.overrideFn).toStrictEqual({
      kind: 'named',
      name: CRUD.CONFIG.operations.get.overrideFn.import,
      path: CRUD.CONFIG.operations.get.overrideFn.from,
    })
    expect(crudDecl.declValue.operations.getAll).toBeDefined()
    expect(crudDecl.declValue.operations.getAll?.isPublic).toBe(
      CRUD.CONFIG.operations.getAll.isPublic
    )
    expect(crudDecl.declValue.operations.getAll?.overrideFn).toStrictEqual({
      kind: 'named',
      name: CRUD.CONFIG.operations.getAll.overrideFn.import,
      path: CRUD.CONFIG.operations.getAll.overrideFn.from,
    })
    expect(crudDecl.declValue.operations.create).toBeDefined()
    expect(crudDecl.declValue.operations.create?.isPublic).toBe(
      CRUD.CONFIG.operations.create.isPublic
    )
    expect(crudDecl.declValue.operations.create?.overrideFn).toStrictEqual({
      kind: 'named',
      name: CRUD.CONFIG.operations.create.overrideFn.import,
      path: CRUD.CONFIG.operations.create.overrideFn.from,
    })
    expect(crudDecl.declValue.operations.update).toBeDefined()
    expect(crudDecl.declValue.operations.update?.isPublic).toBe(
      CRUD.CONFIG.operations.update.isPublic
    )
    expect(crudDecl.declValue.operations.update?.overrideFn).toStrictEqual({
      kind: 'named',
      name: CRUD.CONFIG.operations.update.overrideFn.import,
      path: CRUD.CONFIG.operations.update.overrideFn.from,
    })
    expect(crudDecl.declValue.operations.delete).toBeDefined()
    expect(crudDecl.declValue.operations.delete?.isPublic).toBe(
      CRUD.CONFIG.operations.delete.isPublic
    )
    expect(crudDecl.declValue.operations.delete?.overrideFn).toStrictEqual({
      kind: 'named',
      name: CRUD.CONFIG.operations.delete.overrideFn.import,
      path: CRUD.CONFIG.operations.delete.overrideFn.from,
    })

    // JobConfig
    expect(declTypes).toContain('Job')
    expect(declNames).toContain(JOB.NAME)

    const jobDecl = result.find(
      (decl) => decl.declType === 'Job' && decl.declName === JOB.NAME
    ) as AppSpec.GetDeclForType<'Job'>

    expect(jobDecl.declValue.executor).toBe(JOB.CONFIG.executor)
    expect(jobDecl.declValue.perform.fn).toStrictEqual({
      kind: 'named',
      name: JOB.CONFIG.perform.fn.import,
      path: JOB.CONFIG.perform.fn.from,
    })
    expect(jobDecl.declValue.entities?.[0]).toStrictEqual({
      name: JOB.CONFIG.entities[0],
      declType: 'Entity',
    })
    expect(jobDecl.declValue.schedule).toStrictEqual({
      cron: JOB.CONFIG.schedule.cron,
      args: JOB.CONFIG.schedule.args,
      executorOptions: {
        pgBoss: JOB.CONFIG.schedule.executorOptions.pgBoss,
      },
    })

    // PageConfig
    expect(declTypes).toContain('Page')

    // Login Page
    expect(declNames).toContain(PAGE.LOGIN.NAME)

    const loginPageDecl = result.find(
      (decl) => decl.declType === 'Page' && decl.declName === PAGE.LOGIN.NAME
    ) as AppSpec.GetDeclForType<'Page'>

    expect(loginPageDecl.declValue.component).toStrictEqual({
      kind: 'named',
      name: PAGE.LOGIN.CONFIG.component.import,
      path: PAGE.LOGIN.CONFIG.component.from,
    })
    expect(loginPageDecl.declValue.authRequired).toBe(
      PAGE.LOGIN.CONFIG.authRequired
    )

    // Email Verification Page
    expect(declNames).toContain(PAGE.EMAIL_VERIFICATION.NAME)

    const emailVerificationPageDecl = result.find(
      (decl) =>
        decl.declType === 'Page' &&
        decl.declName === PAGE.EMAIL_VERIFICATION.NAME
    ) as AppSpec.GetDeclForType<'Page'>

    expect(emailVerificationPageDecl.declValue.component).toStrictEqual({
      kind: 'named',
      name: PAGE.EMAIL_VERIFICATION.CONFIG.component.import,
      path: PAGE.EMAIL_VERIFICATION.CONFIG.component.from,
    })
    expect(emailVerificationPageDecl.declValue.authRequired).toBe(
      PAGE.EMAIL_VERIFICATION.CONFIG.authRequired
    )

    // Password Reset Page
    expect(declNames).toContain(PAGE.PASSWORD_RESET.NAME)

    const passwordResetPageDecl = result.find(
      (decl) =>
        decl.declType === 'Page' && decl.declName === PAGE.PASSWORD_RESET.NAME
    ) as AppSpec.GetDeclForType<'Page'>

    expect(passwordResetPageDecl.declValue.component).toStrictEqual({
      kind: 'named',
      name: PAGE.PASSWORD_RESET.CONFIG.component.import,
      path: PAGE.PASSWORD_RESET.CONFIG.component.from,
    })
    expect(passwordResetPageDecl.declValue.authRequired).toBe(
      PAGE.PASSWORD_RESET.CONFIG.authRequired
    )

    // RouteConfig
    expect(declTypes).toContain('Route')

    // Login Route
    expect(declNames).toContain(ROUTE.LOGIN.NAME)

    const routeDecl = result.find(
      (decl) => decl.declType === 'Route' && decl.declName === ROUTE.LOGIN.NAME
    ) as AppSpec.GetDeclForType<'Route'>

    expect(routeDecl.declValue.path).toBe(ROUTE.LOGIN.CONFIG.path)
    expect(routeDecl.declValue.to).toStrictEqual({
      name: PAGE.LOGIN.NAME,
      declType: 'Page',
    })

    // Email Verification Route
    expect(declNames).toContain(ROUTE.EMAIL_VERIFICATION.NAME)

    const emailVerificationRouteDecl = result.find(
      (decl) =>
        decl.declType === 'Route' &&
        decl.declName === ROUTE.EMAIL_VERIFICATION.NAME
    ) as AppSpec.GetDeclForType<'Route'>

    expect(emailVerificationRouteDecl.declValue.path).toBe(
      ROUTE.EMAIL_VERIFICATION.CONFIG.path
    )
    expect(emailVerificationRouteDecl.declValue.to).toStrictEqual({
      name: PAGE.EMAIL_VERIFICATION.NAME,
      declType: 'Page',
    })

    // Password Reset Route
    expect(declNames).toContain(ROUTE.PASSWORD_RESET.NAME)

    const passwordResetRouteDecl = result.find(
      (decl) =>
        decl.declType === 'Route' && decl.declName === ROUTE.PASSWORD_RESET.NAME
    ) as AppSpec.GetDeclForType<'Route'>

    expect(passwordResetRouteDecl.declValue.path).toBe(
      ROUTE.PASSWORD_RESET.CONFIG.path
    )
    expect(passwordResetRouteDecl.declValue.to).toStrictEqual({
      name: PAGE.PASSWORD_RESET.NAME,
      declType: 'Page',
    })

    // QueryConfig
    expect(declTypes).toContain('Query')
    expect(declNames).toContain(QUERY.NAME)

    const queryDecl = result.find(
      (decl) => decl.declType === 'Query' && decl.declName === QUERY.NAME
    ) as AppSpec.GetDeclForType<'Query'>

    expect(queryDecl.declValue.fn).toStrictEqual({
      kind: 'named',
      name: QUERY.CONFIG.fn.import,
      path: QUERY.CONFIG.fn.from,
    })
    expect(queryDecl.declValue.auth).toBe(QUERY.CONFIG.auth)
    expect(queryDecl.declValue.entities?.[0]).toStrictEqual({
      name: QUERY.CONFIG.entities[0],
      declType: 'Entity',
    })
  })

  describe('mapApp', () => {
    test('should map minimal config correctly', () => {
      const app = new UserSpec.App(APP.NAME, {
        title: APP.CONFIG.title,
        wasp: APP.CONFIG.wasp,
      })
      const userSpec = getUserSpec(app)
      const parseEntityRef = makeRefParser('Entity', [])
      const parseRouteRef = makeRefParser('Route', [])

      const result = mapApp(
        userSpec.app.config,
        parseEntityRef,
        parseRouteRef,
        userSpec.auth,
        userSpec.client,
        userSpec.server,
        userSpec.db,
        userSpec.emailSender,
        userSpec.websocket
      )

      expect(result).toBeDefined()
      expect(result).toStrictEqual({
        wasp: {
          version: APP.CONFIG.wasp.version,
        },
        title: APP.CONFIG.title,
        head: undefined,
        auth: undefined,
        server: undefined,
        client: undefined,
        db: undefined,
        emailSender: undefined,
        webSocket: undefined,
      } satisfies AppSpec.App)
    })

    test('should map full config correctly', () => {
      const app = new UserSpec.App(APP.NAME, APP.CONFIG)
      app.auth(AUTH.CONFIG)
      app.server(SERVER.CONFIG)
      app.client(CLIENT.CONFIG)
      app.db(DB.CONFIG)
      app.emailSender(EMAIL.CONFIG)
      app.webSocket(WEBSOCKET.CONFIG)

      const userSpec = getUserSpec(app)
      const parseEntityRef = makeRefParser('Entity', [
        AUTH.CONFIG.userEntity,
        AUTH.CONFIG.externalAuthEntity,
      ])
      const parseRouteRef = makeRefParser('Route', [
        AUTH.CONFIG.methods.email.emailVerification.clientRoute,
        AUTH.CONFIG.methods.email.passwordReset.clientRoute,
      ])

      const result = mapApp(
        userSpec.app.config,
        parseEntityRef,
        parseRouteRef,
        userSpec.auth,
        userSpec.server,
        userSpec.client,
        userSpec.db,
        userSpec.emailSender,
        userSpec.websocket
      )

      expect(result).toStrictEqual({
        wasp: {
          version: APP.CONFIG.wasp.version,
        },
        title: APP.CONFIG.title,
        head: APP.CONFIG.head,
        auth:
          userSpec.auth &&
          mapAuth(userSpec.auth, parseEntityRef, parseRouteRef),
        server: userSpec.server && mapServer(userSpec.server),
        client: userSpec.client && mapClient(userSpec.client),
        db: userSpec.db && mapDb(userSpec.db),
        emailSender:
          userSpec.emailSender && mapEmailSender(userSpec.emailSender),
        webSocket: userSpec.websocket && mapWebSocket(userSpec.websocket),
      } satisfies AppSpec.App)
    })
  })

  describe('mapExtImport', () => {
    test('should map named import', () => {
      const extImport: UserSpec.ExtImport = {
        import: 'myNamedImport',
        from: '@src/myModule',
      }
      const result = mapExtImport(extImport)
      expect(result).toStrictEqual({
        kind: 'named',
        name: 'myNamedImport',
        path: '@src/myModule',
      })
    })

    test('should map default import', () => {
      const extImport: UserSpec.ExtImport = {
        importDefault: 'myDefaultImport',
        from: '@src/myModule',
      }
      const result = mapExtImport(extImport)
      expect(result).toStrictEqual({
        kind: 'default',
        name: 'myDefaultImport',
        path: '@src/myModule',
      })
    })

    test('should throw error for having both import kind', () => {
      const extImport: UserSpec.ExtImport = {
        import: 'myNamedImport',
        from: '@src/myModule',
        importDefault: 'myDefaultImport',
      }
      expect(() => mapExtImport(extImport)).toThrowError(
        'Invalid ExtImport: both `import` and `importDefault` are defined'
      )
    })

    test('should throw error for missing import kind', () => {
      const extImport: UserSpec.ExtImport = {
        from: '@src/myModule',
      } as unknown as UserSpec.ExtImport
      expect(() => mapExtImport(extImport)).toThrowError(
        'Invalid ExtImport: neither `import` nor `importDefault` is defined'
      )
    })

    test('should throw error for invalid from path', () => {
      const extImport: UserSpec.ExtImport = {
        import: 'myNamedImport',
        from: 'invalid/path',
      } as unknown as UserSpec.ExtImport
      expect(() => mapExtImport(extImport)).toThrowError(
        `Invalid ExtImport: path must start with '@src/', got ${extImport.from}`
      )
    })
  })

  describe('mapAuth', () => {
    test('should map minimal config correctly', () => {
      const minimalAuthConfig: UserSpec.AuthConfig = {
        userEntity: AUTH.CONFIG.userEntity,
        methods: {},
        onAuthFailedRedirectTo: AUTH.CONFIG.onAuthFailedRedirectTo,
      }
      const parseEntityRef = makeRefParser('Entity', [
        minimalAuthConfig.userEntity,
      ])
      const parseRouteRef = makeRefParser('Route', [])

      const result = mapAuth(minimalAuthConfig, parseEntityRef, parseRouteRef)

      expect(result).toStrictEqual({
        userEntity: parseEntityRef(AUTH.CONFIG.userEntity),
        externalAuthEntity: undefined,
        methods: mapAuthMethods(minimalAuthConfig.methods, parseRouteRef),
        onAuthFailedRedirectTo: AUTH.CONFIG.onAuthFailedRedirectTo,
        onAuthSucceededRedirectTo: undefined,
        onBeforeSignup: undefined,
        onAfterSignup: undefined,
        onBeforeOAuthRedirect: undefined,
        onBeforeLogin: undefined,
        onAfterLogin: undefined,
      } satisfies AppSpec.Auth)
    })

    test('should map full config correctly', () => {
      const authConfig = AUTH.CONFIG
      const parseEntityRef = makeRefParser('Entity', [
        authConfig.userEntity,
        authConfig.externalAuthEntity,
      ])
      const parseRouteRef = makeRefParser('Route', [
        authConfig.methods.email.emailVerification.clientRoute,
        authConfig.methods.email.passwordReset.clientRoute,
      ])

      const result = mapAuth(authConfig, parseEntityRef, parseRouteRef)

      expect(result).toStrictEqual({
        userEntity: parseEntityRef(authConfig.userEntity),
        externalAuthEntity: parseEntityRef(authConfig.externalAuthEntity),
        methods: mapAuthMethods(authConfig.methods, parseRouteRef),
        onAuthFailedRedirectTo: authConfig.onAuthFailedRedirectTo,
        onAuthSucceededRedirectTo: authConfig.onAuthSucceededRedirectTo,
        onBeforeSignup: mapExtImport(authConfig.onBeforeSignup),
        onAfterSignup: mapExtImport(authConfig.onAfterSignup),
        onBeforeOAuthRedirect: mapExtImport(authConfig.onBeforeOAuthRedirect),
        onBeforeLogin: mapExtImport(authConfig.onBeforeLogin),
        onAfterLogin: mapExtImport(authConfig.onAfterLogin),
      } satisfies AppSpec.Auth)
    })

    test('should throw if userEntity is not provided', () => {
      const authConfig: UserSpec.AuthConfig = {
        userEntity: AUTH.CONFIG.userEntity,
        methods: {},
        onAuthFailedRedirectTo: AUTH.CONFIG.onAuthFailedRedirectTo,
      }
      const parseEntityRef = makeRefParser('Entity', [])
      const parseRouteRef = makeRefParser('Route', [])

      expect(() =>
        mapAuth(authConfig, parseEntityRef, parseRouteRef)
      ).toThrowError(`Invalid Entity reference: ${authConfig.userEntity}`)
    })

    test('should throw if externalAuthEntity ref is not provided when defined', () => {
      const authConfig: UserSpec.AuthConfig = {
        userEntity: AUTH.CONFIG.userEntity,
        externalAuthEntity: AUTH.CONFIG.externalAuthEntity,
        methods: {},
        onAuthFailedRedirectTo: AUTH.CONFIG.onAuthFailedRedirectTo,
      }
      const parseEntityRef = makeRefParser('Entity', [AUTH.CONFIG.userEntity])
      const parseRouteRef = makeRefParser('Route', [])

      expect(() =>
        mapAuth(authConfig, parseEntityRef, parseRouteRef)
      ).toThrowError(
        `Invalid Entity reference: ${authConfig.externalAuthEntity}`
      )
    })
  })

  describe('mapAuthMethods', () => {
    test('should map minimal config correctly', () => {
      const minimalAuthMethods: UserSpec.AuthMethods = {}
      const parseRouteRef = makeRefParser('Route', [])

      const result = mapAuthMethods(minimalAuthMethods, parseRouteRef)

      expect(result).toStrictEqual({
        usernameAndPassword: undefined,
        discord: undefined,
        google: undefined,
        gitHub: undefined,
        keycloak: undefined,
        email: undefined,
      } satisfies AppSpec.AuthMethods)
    })

    test('should map full config correctly', () => {
      const authMethods = AUTH.CONFIG.methods
      const parseRouteRef = makeRefParser('Route', [
        authMethods.email.emailVerification.clientRoute,
        authMethods.email.passwordReset.clientRoute,
      ])

      const result = mapAuthMethods(authMethods, parseRouteRef)

      expect(result).toStrictEqual({
        usernameAndPassword: mapUsernameAndPassword(
          authMethods.usernameAndPassword
        ),
        discord: mapExternalAuth(authMethods.discord),
        google: mapExternalAuth(authMethods.google),
        gitHub: mapExternalAuth(authMethods.gitHub),
        keycloak: mapExternalAuth(authMethods.keycloak),
        email:
          authMethods.email && mapEmailAuth(authMethods.email, parseRouteRef),
      } satisfies AppSpec.AuthMethods)
    })
  })

  describe('mapEmailAuth', () => {
    test('should map minimal config correctly', () => {
      const minimalEmailAuthConfig: UserSpec.EmailAuthConfig = {
        fromField: AUTH.CONFIG.methods.email.fromField,
        emailVerification: AUTH.CONFIG.methods.email.emailVerification,
        passwordReset: AUTH.CONFIG.methods.email.passwordReset,
      }
      const parseRouteRef = makeRefParser('Route', [
        minimalEmailAuthConfig.emailVerification.clientRoute,
        minimalEmailAuthConfig.passwordReset.clientRoute,
      ])

      const result = mapEmailAuth(minimalEmailAuthConfig, parseRouteRef)

      expect(result).toStrictEqual({
        fromField: {
          name: minimalEmailAuthConfig.fromField.name,
          email: minimalEmailAuthConfig.fromField.email,
        },
        emailVerification: mapEmailVerification(
          minimalEmailAuthConfig.emailVerification,
          parseRouteRef
        ),
        passwordReset: mapPasswordReset(
          minimalEmailAuthConfig.passwordReset,
          parseRouteRef
        ),
        userSignupFields: undefined,
      } satisfies AppSpec.EmailAuthConfig)
    })

    test('should map full config correctly', () => {
      const emailAuthConfig = AUTH.CONFIG.methods.email
      const parseRouteRef = makeRefParser('Route', [
        emailAuthConfig.emailVerification.clientRoute,
        emailAuthConfig.passwordReset.clientRoute,
      ])

      const result = mapEmailAuth(emailAuthConfig, parseRouteRef)

      expect(result).toStrictEqual({
        userSignupFields: mapExtImport(emailAuthConfig.userSignupFields),
        fromField: {
          name: emailAuthConfig.fromField.name,
          email: emailAuthConfig.fromField.email,
        },
        emailVerification: mapEmailVerification(
          emailAuthConfig.emailVerification,
          parseRouteRef
        ),
        passwordReset: mapPasswordReset(
          emailAuthConfig.passwordReset,
          parseRouteRef
        ),
      } satisfies AppSpec.EmailAuthConfig)
    })

    test('should throw if email verification client route is not provided when defined', () => {
      const emailAuthConfig: UserSpec.EmailAuthConfig = {
        ...AUTH.CONFIG.methods.email,
        emailVerification: {
          getEmailContentFn:
            AUTH.CONFIG.methods.email.emailVerification.getEmailContentFn,
          clientRoute: 'undefined',
        },
      }
      const parseRouteRef = makeRefParser('Route', [
        emailAuthConfig.passwordReset.clientRoute,
      ])

      expect(() => mapEmailAuth(emailAuthConfig, parseRouteRef)).toThrowError(
        `Invalid Route reference: undefined`
      )
    })

    test('should throw if password reset client route is not provided when defined', () => {
      const emailAuthConfig: UserSpec.EmailAuthConfig = {
        ...AUTH.CONFIG.methods.email,
        passwordReset: {
          getEmailContentFn:
            AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn,
          clientRoute: 'undefined',
        },
      }
      const parseRouteRef = makeRefParser('Route', [
        emailAuthConfig.emailVerification.clientRoute,
      ])

      expect(() => mapEmailAuth(emailAuthConfig, parseRouteRef)).toThrowError(
        `Invalid Route reference: undefined`
      )
    })
  })

  describe('mapEmailVerification', () => {
    test('should map minimal config correctly', () => {
      const minimalEmailVerificationConfig: UserSpec.EmailVerificationConfig = {
        clientRoute: AUTH.CONFIG.methods.email.emailVerification.clientRoute,
      }
      const parseRouteRef = makeRefParser('Route', [
        minimalEmailVerificationConfig.clientRoute,
      ])

      const result = mapEmailVerification(
        minimalEmailVerificationConfig,
        parseRouteRef
      )

      expect(result).toStrictEqual({
        getEmailContentFn: undefined,
        clientRoute: parseRouteRef(
          AUTH.CONFIG.methods.email.emailVerification.clientRoute
        ),
      } satisfies AppSpec.EmailVerificationConfig)
    })

    test('should map full config correctly', () => {
      const emailVerificationConfig =
        AUTH.CONFIG.methods.email.emailVerification
      const parseRouteRef = makeRefParser('Route', [
        emailVerificationConfig.clientRoute,
      ])

      const result = mapEmailVerification(
        emailVerificationConfig,
        parseRouteRef
      )

      expect(result).toStrictEqual({
        getEmailContentFn: mapExtImport(
          emailVerificationConfig.getEmailContentFn
        ),
        clientRoute: parseRouteRef(emailVerificationConfig.clientRoute),
      } satisfies AppSpec.EmailVerificationConfig)
    })
  })

  describe('mapPasswordReset', () => {
    test('should map minimal config correctly', () => {
      const minimalPasswordResetConfig: UserSpec.PasswordResetConfig = {
        clientRoute: AUTH.CONFIG.methods.email.passwordReset.clientRoute,
      }
      const parseRouteRef = makeRefParser('Route', [
        minimalPasswordResetConfig.clientRoute,
      ])

      const result = mapPasswordReset(minimalPasswordResetConfig, parseRouteRef)

      expect(result).toStrictEqual({
        getEmailContentFn: undefined,
        clientRoute: parseRouteRef(
          AUTH.CONFIG.methods.email.passwordReset.clientRoute
        ),
      } satisfies AppSpec.PasswordResetConfig)
    })

    test('should map full config correctly', () => {
      const passwordResetConfig = AUTH.CONFIG.methods.email.passwordReset
      const parseRouteRef = makeRefParser('Route', [
        AUTH.CONFIG.methods.email.passwordReset.clientRoute,
      ])

      const result = mapPasswordReset(passwordResetConfig, parseRouteRef)

      expect(result).toStrictEqual({
        getEmailContentFn: mapExtImport(
          AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn
        ),
        clientRoute: parseRouteRef(
          AUTH.CONFIG.methods.email.passwordReset.clientRoute
        ),
      } satisfies AppSpec.PasswordResetConfig)
    })
  })

  describe('mapUsernameAndPassword', () => {
    test('should map minimal config correctly', () => {
      const minimalUsernameAndPassword: UserSpec.UsernameAndPasswordConfig = {}

      const result = mapUsernameAndPassword(minimalUsernameAndPassword)

      expect(result).toStrictEqual({
        userSignupFields: undefined,
      } satisfies AppSpec.UsernameAndPasswordConfig)
    })

    test('should map full config correctly', () => {
      const usernameAndPassword = AUTH.CONFIG.methods.usernameAndPassword

      const result = mapUsernameAndPassword(usernameAndPassword)

      expect(result).toStrictEqual({
        userSignupFields: mapExtImport(usernameAndPassword.userSignupFields),
      } satisfies AppSpec.UsernameAndPasswordConfig)
    })
  })

  describe('mapExternalAuth', () => {
    test('should map minimal config correctly', () => {
      const minimalExternalAuthConfig: UserSpec.ExternalAuthConfig = {}

      const result = mapExternalAuth(minimalExternalAuthConfig)

      expect(result).toStrictEqual({
        configFn: undefined,
        userSignupFields: undefined,
      } satisfies AppSpec.ExternalAuthConfig)
    })

    test('should map full config correctly', () => {
      const externalAuthConfig = AUTH.CONFIG.methods.discord

      const result = mapExternalAuth(externalAuthConfig)

      expect(result).toStrictEqual({
        configFn: mapExtImport(externalAuthConfig.configFn),
        userSignupFields: mapExtImport(externalAuthConfig.userSignupFields),
      } satisfies AppSpec.ExternalAuthConfig)
    })
  })

  describe('mapDb', () => {
    test('should map minimal config correctly', () => {
      const minimalDbConfig: UserSpec.DbConfig = {}

      const result = mapDb(minimalDbConfig)

      expect(result).toStrictEqual({
        seeds: undefined,
      } satisfies AppSpec.Db)
    })

    test('should map full config correctly', () => {
      const dbConfig: UserSpec.DbConfig = {
        seeds: DB.CONFIG.seeds,
      }

      const result = mapDb(dbConfig)

      expect(result).toStrictEqual({
        seeds: dbConfig.seeds?.map((seed) => mapExtImport(seed)),
      } satisfies AppSpec.Db)
    })
  })

  describe('mapEmailSender', () => {
    test('should map minimal config correctly', () => {
      const emailSender: UserSpec.EmailSender = {
        provider: EMAIL.CONFIG.provider,
      }

      const result = mapEmailSender(emailSender)

      expect(result).toStrictEqual({
        provider: emailSender.provider,
        defaultFrom: undefined,
      } satisfies AppSpec.EmailSender)
    })

    test('should map full config correctly', () => {
      const emailSenderConfig = EMAIL.CONFIG

      const result = mapEmailSender(emailSenderConfig)

      expect(result).toStrictEqual({
        provider: emailSenderConfig.provider,
        defaultFrom: emailSenderConfig.defaultFrom,
      } satisfies AppSpec.EmailSender)
    })
  })

  describe('mapServer', () => {
    test('should map minimal config correctly', () => {
      const minimalServerConfig: UserSpec.ServerConfig = {}

      const result = mapServer(minimalServerConfig)

      expect(result).toStrictEqual({
        setupFn: undefined,
        middlewareConfigFn: undefined,
        envValidationSchema: undefined,
      } satisfies AppSpec.Server)
    })

    test('should map full config correctly', () => {
      const serverConfig = SERVER.CONFIG

      const result = mapServer(serverConfig)

      expect(result).toStrictEqual({
        setupFn: mapExtImport(serverConfig.setupFn),
        middlewareConfigFn: mapExtImport(serverConfig.middlewareConfigFn),
        envValidationSchema: mapExtImport(serverConfig.envValidationSchema),
      } satisfies AppSpec.Server)
    })
  })

  describe('mapWebSocket', () => {
    test('should map minimal config correctly', () => {
      const minimalWebsocketConfig: UserSpec.WebsocketConfig = {
        fn: WEBSOCKET.CONFIG.fn,
      }

      const result = mapWebSocket(minimalWebsocketConfig)

      expect(result).toStrictEqual({
        fn: mapExtImport(minimalWebsocketConfig.fn),
        autoConnect: undefined,
      } satisfies AppSpec.WebSocket)
    })

    test('should map full config correctly', () => {
      const websocketConfig = WEBSOCKET.CONFIG

      const result = mapWebSocket(websocketConfig)

      expect(result).toStrictEqual({
        fn: mapExtImport(websocketConfig.fn),
        autoConnect: websocketConfig.autoConnect,
      } satisfies AppSpec.WebSocket)
    })
  })

  describe('mapClient', () => {
    test('should map minimal config correctly', () => {
      const minimalClientConfig: UserSpec.ClientConfig = {}

      const result = mapClient(minimalClientConfig)

      expect(result).toStrictEqual({
        rootComponent: undefined,
        setupFn: undefined,
        baseDir: undefined,
        envValidationSchema: undefined,
      } satisfies AppSpec.Client)
    })

    test('should map full config correctly', () => {
      const clientConfig = CLIENT.CONFIG

      const result = mapClient(clientConfig)

      expect(result).toStrictEqual({
        rootComponent: mapExtImport(clientConfig.rootComponent),
        setupFn: mapExtImport(clientConfig.setupFn),
        baseDir: clientConfig.baseDir,
        envValidationSchema: mapExtImport(clientConfig.envValidationSchema),
      } satisfies AppSpec.Client)
    })
  })

  describe('mapOperationConfig', () => {
    test('should map minimal query config correctly', () => {
      const minimalQueryConfig: UserSpec.QueryConfig = {
        fn: QUERY.CONFIG.fn,
      }
      const parseEntityRef = makeRefParser('Entity', [])

      const result = mapOperationConfig(minimalQueryConfig, parseEntityRef)

      expect(result).toStrictEqual({
        fn: mapExtImport(minimalQueryConfig.fn),
        entities: undefined,
        auth: undefined,
      } satisfies AppSpec.Query)
    })

    test('should map query config correctly', () => {
      const queryConfig = QUERY.CONFIG
      const parseEntityRef = makeRefParser('Entity', queryConfig.entities)

      const result = mapOperationConfig(queryConfig, parseEntityRef)

      expect(result).toStrictEqual({
        fn: mapExtImport(queryConfig.fn),
        entities: queryConfig.entities.map(parseEntityRef),
        auth: queryConfig.auth,
      } satisfies AppSpec.Query)
    })

    test('should throw if entity ref is not provided in query config', () => {
      const queryConfig: UserSpec.QueryConfig = QUERY.CONFIG
      const parseEntityRef = makeRefParser('Entity', [])

      expect(() =>
        mapOperationConfig(queryConfig, parseEntityRef)
      ).toThrowError(`Invalid Entity reference: ${queryConfig.entities?.[0]}`)
    })

    test('should map minimal action config correctly', () => {
      const minimalActionConfig: UserSpec.ActionConfig = {
        fn: ACTION.CONFIG.fn,
      }
      const parseEntityRef = makeRefParser('Entity', [])

      const result = mapOperationConfig(minimalActionConfig, parseEntityRef)

      expect(result).toStrictEqual({
        fn: mapExtImport(minimalActionConfig.fn),
        entities: undefined,
        auth: undefined,
      } satisfies AppSpec.Action)
    })

    test('should map action config correctly', () => {
      const actionConifg = ACTION.CONFIG
      const parseEntityRef = makeRefParser('Entity', actionConifg.entities)

      const result = mapOperationConfig(actionConifg, parseEntityRef)

      expect(result).toStrictEqual({
        fn: mapExtImport(actionConifg.fn),
        entities: actionConifg.entities.map(parseEntityRef),
        auth: actionConifg.auth,
      } satisfies AppSpec.Action)
    })

    test('should throw if entity ref is not provided in action config', () => {
      const actionConfig: UserSpec.ActionConfig = ACTION.CONFIG
      const parseEntityRef = makeRefParser('Entity', [])

      expect(() =>
        mapOperationConfig(actionConfig, parseEntityRef)
      ).toThrowError(`Invalid Entity reference: ${actionConfig.entities?.[0]}`)
    })
  })

  describe('mapApiConfig', () => {
    test('should map minimal config correctly', () => {
      const minimalApi: UserSpec.ApiConfig = {
        fn: API.CONFIG.fn,
        httpRoute: API.CONFIG.httpRoute,
      }
      const parseEntityRef = makeRefParser('Entity', [])

      const result = mapApiConfig(minimalApi, parseEntityRef)

      expect(result).toStrictEqual({
        fn: mapExtImport(minimalApi.fn),
        httpRoute: mapHttpRoute(minimalApi.httpRoute),
        auth: undefined,
        entities: undefined,
        middlewareConfigFn: undefined,
      } satisfies AppSpec.Api)
    })

    test('should map full config correctly', () => {
      const api = API.CONFIG
      const parseEntityRef = makeRefParser('Entity', api.entities)

      const result = mapApiConfig(api, parseEntityRef)

      expect(result).toStrictEqual({
        fn: mapExtImport(api.fn),
        middlewareConfigFn: mapExtImport(api.middlewareConfigFn),
        entities: api.entities.map(parseEntityRef),
        httpRoute: mapHttpRoute(api.httpRoute),
        auth: api.auth,
      } satisfies AppSpec.Api)
    })
  })

  describe('mapHttpRoute', () => {
    test('should map full config correctly', () => {
      const httpRoute = API.CONFIG.httpRoute

      const result = mapHttpRoute(httpRoute)

      expect(result).toStrictEqual([
        httpRoute.method,
        httpRoute.route,
      ] satisfies AppSpec.HttpRoute)
    })
  })

  describe('mapApiNamespace', () => {
    test('should map full config correctly', () => {
      const apiNamespace = API_NAMESPACE.CONFIG

      const result = mapApiNamespace(apiNamespace)

      expect(result).toStrictEqual({
        middlewareConfigFn: mapExtImport(apiNamespace.middlewareConfigFn),
        path: apiNamespace.path,
      } satisfies AppSpec.ApiNamespace)
    })
  })

  describe('mapJob', () => {
    test('should map full config correctly', () => {
      const job = JOB.CONFIG
      const parseEntityRef = makeRefParser('Entity', job.entities)

      const result = mapJob(job, parseEntityRef)

      expect(result).toStrictEqual({
        executor: job.executor,
        perform: mapPerform(job.perform),
        schedule: mapSchedule(job.schedule),
        entities: job.entities.map(parseEntityRef),
      } satisfies AppSpec.Job)
    })

    test('should throw if entity ref is not provided', () => {
      const job: UserSpec.JobConfig = JOB.CONFIG
      const parseEntityRef = makeRefParser('Entity', [])

      expect(() => mapJob(job, parseEntityRef)).toThrowError(
        `Invalid Entity reference: ${job.entities?.[0]}`
      )
    })
  })

  describe('mapSchedule', () => {
    test('should map full config correctly', () => {
      const schedule = JOB.CONFIG.schedule

      const result = mapSchedule(schedule)

      expect(result).toStrictEqual({
        cron: schedule.cron,
        args: schedule.args,
        executorOptions: JOB.CONFIG.perform.executorOptions,
      } satisfies AppSpec.Schedule)
    })
  })

  describe('mapPerform', () => {
    test('should map full config correctly', () => {
      const perform = JOB.CONFIG.perform

      const result = mapPerform(perform)

      expect(result).toStrictEqual({
        fn: mapExtImport(perform.fn),
        executorOptions: perform.executorOptions,
      } satisfies AppSpec.Perform)
    })
  })

  describe('mapRoute', () => {
    test('should map full config correctly', () => {
      const route = ROUTE.LOGIN.CONFIG
      const parsePageRef = makeRefParser('Page', [route.to])

      const result = mapRoute(route, parsePageRef)

      expect(result).toStrictEqual({
        path: route.path,
        to: parsePageRef(route.to),
      } satisfies AppSpec.Route)
    })
  })

  describe('mapCrud', () => {
    test('should map minimal config correctly', () => {
      const minimalCrud: UserSpec.Crud = {
        entity: CRUD.CONFIG.entity,
        operations: {},
      }
      const parseEntityRef = makeRefParser('Entity', [minimalCrud.entity])

      const result = mapCrud(minimalCrud, parseEntityRef)

      expect(result).toStrictEqual({
        entity: parseEntityRef(minimalCrud.entity),
        operations: mapCrudOperations({}),
      } satisfies AppSpec.Crud)
    })

    test('should map full config correctly', () => {
      const crud = CRUD.CONFIG
      const parseEntityRef = makeRefParser('Entity', [crud.entity])

      const result = mapCrud(crud, parseEntityRef)

      expect(result).toStrictEqual({
        entity: parseEntityRef(crud.entity),
        operations: mapCrudOperations(crud.operations),
      } satisfies AppSpec.Crud)
    })

    test('should throw if entity ref is not provided', () => {
      const crud: UserSpec.Crud = CRUD.CONFIG
      const parseEntityRef = makeRefParser('Entity', [])

      expect(() => mapCrud(crud, parseEntityRef)).toThrowError(
        `Invalid Entity reference: ${CRUD.CONFIG.entity}`
      )
    })
  })

  describe('mapCrudOperations', () => {
    test('should map minimal config correctly', () => {
      const minimalCrudOperations: UserSpec.CrudOperations = {}

      const result = mapCrudOperations(minimalCrudOperations)

      expect(result).toStrictEqual({
        get: undefined,
        getAll: undefined,
        create: undefined,
        update: undefined,
        delete: undefined,
      } satisfies AppSpec.CrudOperations)
    })

    test('should map full config correctly', () => {
      const crudOperations = CRUD.CONFIG.operations

      const result = mapCrudOperations(crudOperations)

      expect(result).toStrictEqual({
        get: mapCrudOperationOptions(crudOperations.get),
        getAll: mapCrudOperationOptions(crudOperations.getAll),
        create: mapCrudOperationOptions(crudOperations.create),
        update: mapCrudOperationOptions(crudOperations.update),
        delete: mapCrudOperationOptions(crudOperations.delete),
      } satisfies AppSpec.CrudOperations)
    })
  })

  describe('mapCrudOperationOptions', () => {
    test('should map minimal config correctly', () => {
      const minimalCrudOperationOptions: UserSpec.CrudOperationOptions = {
        isPublic: false,
      }

      const result = mapCrudOperationOptions(minimalCrudOperationOptions)

      expect(result).toStrictEqual({
        isPublic: false,
        overrideFn: undefined,
      } satisfies AppSpec.CrudOperationOptions)
    })

    test('should map full config correctly', () => {
      const crudOperationOptions = CRUD.CONFIG.operations.get

      const result = mapCrudOperationOptions(crudOperationOptions)

      expect(result).toStrictEqual({
        isPublic: crudOperationOptions.isPublic,
        overrideFn: mapExtImport(crudOperationOptions.overrideFn),
      } satisfies AppSpec.CrudOperationOptions)
    })
  })

  describe('mapPage', () => {
    test('should map minimal config correctly', () => {
      const minimalPage: UserSpec.PageConfig = {
        component: PAGE.LOGIN.CONFIG.component,
      }

      const result = mapPage(minimalPage)

      expect(result).toStrictEqual({
        component: mapExtImport(minimalPage.component),
        authRequired: undefined,
      } satisfies AppSpec.Page)
    })

    test('should map full config correctly', () => {
      const page = PAGE.LOGIN.CONFIG

      const result = mapPage(page)

      expect(result).toStrictEqual({
        component: mapExtImport(page.component),
        authRequired: page.authRequired,
      } satisfies AppSpec.Page)
    })
  })
})
