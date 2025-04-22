import { describe, expect, test } from 'vitest'
import * as AppSpec from '../appSpec.js'
import { getUserSpec } from '../mapUserSpecToAppSepcJson.js'
import {
  makeRefParser,
  mapApp,
  mapAuth,
  mapAuthMethods,
  mapClient,
  mapDb,
  mapEmailAuth,
  mapEmailSender,
  mapEmailVerification,
  mapExternalAuth,
  mapExtImport,
  mapPasswordReset,
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

    // AppConfig Mapping
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

    // AuthConfig Mapping
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
    // Hooks
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

    // ClientConfig Mapping
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

    // DbConfig Mapping
    const db = appDecl.declValue.db
    expect(db).toBeDefined()
    expect(db?.seeds).toHaveLength(1)
    expect(db?.seeds?.[0]).toStrictEqual({
      kind: 'named',
      name: DB.CONFIG.seeds[0]?.import,
      path: DB.CONFIG.seeds[0]?.from,
    })

    // EmailSenderConfig Mapping
    const emailSender = appDecl.declValue.emailSender
    expect(emailSender).toBeDefined()
    expect(emailSender?.provider).toBe(EMAIL.CONFIG.provider)
    expect(emailSender?.defaultFrom?.email).toBe(EMAIL.CONFIG.defaultFrom.email)
    expect(emailSender?.defaultFrom?.name).toBe(EMAIL.CONFIG.defaultFrom.name)

    // ServerConfig Mapping
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

    // WebSocketConfig Mapping
    const webSocket = appDecl.declValue.webSocket
    expect(webSocket).toBeDefined()
    expect(webSocket?.fn).toStrictEqual({
      kind: 'named',
      name: WEBSOCKET.CONFIG.fn.import,
      path: WEBSOCKET.CONFIG.fn.from,
    })

    // ActionConfig Mapping
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

    // ApiNamespaceConfig Mapping
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

    // ApiConfig Mapping
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

    // CrudConfig Mapping
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
    expect(crudDecl.declValue.operations.create).toBeDefined()
    expect(crudDecl.declValue.operations.create?.isPublic).toBe(
      CRUD.CONFIG.operations.create.isPublic
    )
    expect(crudDecl.declValue.operations.update).toBeDefined()
    expect(crudDecl.declValue.operations.update?.isPublic).toBe(
      CRUD.CONFIG.operations.update.isPublic
    )
    expect(crudDecl.declValue.operations.delete).toBeDefined()
    expect(crudDecl.declValue.operations.delete?.isPublic).toBe(
      CRUD.CONFIG.operations.delete.isPublic
    )
    expect(crudDecl.declValue.operations.delete?.overrideFn).toStrictEqual({
      kind: 'named',
      name: CRUD.CONFIG.operations.delete.overrideFn.import,
      path: CRUD.CONFIG.operations.delete.overrideFn.from,
    })

    // JobConfig Mapping
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

    // PageConfig Mapping
    expect(declTypes).toContain('Page')
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

    // RouteConfig Mapping
    expect(declTypes).toContain('Route')
    expect(declNames).toContain(ROUTE.LOGIN.NAME)
    const routeDecl = result.find(
      (decl) => decl.declType === 'Route' && decl.declName === ROUTE.LOGIN.NAME
    ) as AppSpec.GetDeclForType<'Route'>
    expect(routeDecl.declValue.path).toBe(ROUTE.LOGIN.CONFIG.path)
    expect(routeDecl.declValue.to).toStrictEqual({
      name: PAGE.LOGIN.NAME,
      declType: 'Page',
    })
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

    // QueryConfig Mapping
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
    test('should map minimal UserSpec App', () => {
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
      const auth: UserSpec.AuthConfig = {
        userEntity: AUTH.CONFIG.userEntity,
        methods: {},
        onAuthFailedRedirectTo: AUTH.CONFIG.onAuthFailedRedirectTo,
      }
      const parseEntityRef = makeRefParser('Entity', [AUTH.CONFIG.userEntity])
      const parseRouteRef = makeRefParser('Route', [])

      const result = mapAuth(auth, parseEntityRef, parseRouteRef)
      expect(result).toStrictEqual({
        userEntity: {
          name: AUTH.CONFIG.userEntity,
          declType: 'Entity',
        },
        externalAuthEntity: undefined,
        methods: {
          usernameAndPassword: undefined,
          discord: undefined,
          google: undefined,
          gitHub: undefined,
          keycloak: undefined,
          email: undefined,
        },
        onAuthFailedRedirectTo: AUTH.CONFIG.onAuthFailedRedirectTo,
        onAuthSucceededRedirectTo: undefined,
        onBeforeSignup: undefined,
        onAfterSignup: undefined,
        onBeforeOAuthRedirect: undefined,
        onBeforeLogin: undefined,
        onAfterLogin: undefined,
      } satisfies AppSpec.Auth)
    })

    test('should throw if userEntity is not provided', () => {
      const auth: UserSpec.AuthConfig = {
        userEntity: AUTH.CONFIG.userEntity,
        methods: {},
        onAuthFailedRedirectTo: AUTH.CONFIG.onAuthFailedRedirectTo,
      }
      const parseEntityRef = makeRefParser('Entity', [])
      const parseRouteRef = makeRefParser('Route', [])

      expect(() => mapAuth(auth, parseEntityRef, parseRouteRef)).toThrowError(
        `Invalid Entity reference: ${AUTH.CONFIG.userEntity}`
      )
    })

    test('should throw if externalAuthEntity ref is not provided when defined', () => {
      const auth: UserSpec.AuthConfig = {
        userEntity: AUTH.CONFIG.userEntity,
        externalAuthEntity: AUTH.CONFIG.externalAuthEntity,
        methods: {},
        onAuthFailedRedirectTo: AUTH.CONFIG.onAuthFailedRedirectTo,
      }
      const parseEntityRef = makeRefParser('Entity', [AUTH.CONFIG.userEntity])
      const parseRouteRef = makeRefParser('Route', [])

      expect(() => mapAuth(auth, parseEntityRef, parseRouteRef)).toThrowError(
        `Invalid Entity reference: ${AUTH.CONFIG.externalAuthEntity}`
      )
    })
  })

  describe('mapEmailAuth', () => {
    test('should map minimal config correctly', () => {
      const emailAuth: UserSpec.EmailAuthConfig = {
        fromField: AUTH.CONFIG.methods.email.fromField,
        emailVerification: AUTH.CONFIG.methods.email.emailVerification,
        passwordReset: AUTH.CONFIG.methods.email.passwordReset,
      }
      const parseRouteRef = makeRefParser('Route', [
        AUTH.CONFIG.methods.email.emailVerification.clientRoute,
        AUTH.CONFIG.methods.email.passwordReset.clientRoute,
      ])

      const result = mapEmailAuth(emailAuth, parseRouteRef)

      expect(result).toStrictEqual({
        fromField: {
          name: AUTH.CONFIG.methods.email.fromField.name,
          email: AUTH.CONFIG.methods.email.fromField.email,
        },
        emailVerification: {
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
        },
        passwordReset: {
          getEmailContentFn: {
            kind: 'named',
            name: AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn
              .import,
            path: AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn
              .from,
          },
          clientRoute: {
            name: AUTH.CONFIG.methods.email.passwordReset.clientRoute,
            declType: 'Route',
          },
        },
        userSignupFields: undefined,
      } satisfies AppSpec.EmailAuthConfig)
    })

    test('should map correctly', () => {
      const parseRouteRef = makeRefParser('Route', [
        AUTH.CONFIG.methods.email.emailVerification.clientRoute,
        AUTH.CONFIG.methods.email.passwordReset.clientRoute,
      ])

      const result = mapEmailAuth(AUTH.CONFIG.methods.email, parseRouteRef)

      expect(result).toStrictEqual({
        userSignupFields: {
          kind: 'named',
          name: AUTH.CONFIG.methods.email.userSignupFields.import,
          path: AUTH.CONFIG.methods.email.userSignupFields.from,
        },
        fromField: {
          name: AUTH.CONFIG.methods.email.fromField.name,
          email: AUTH.CONFIG.methods.email.fromField.email,
        },
        emailVerification: {
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
        },
        passwordReset: {
          getEmailContentFn: {
            kind: 'named',
            name: AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn
              .import,
            path: AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn
              .from,
          },
          clientRoute: {
            name: AUTH.CONFIG.methods.email.passwordReset.clientRoute,
            declType: 'Route',
          },
        },
      } satisfies AppSpec.EmailAuthConfig)
    })

    test('should throw if emailVerification clientRoute is not provided when defined', () => {
      const emailAuth: UserSpec.EmailAuthConfig = {
        fromField: AUTH.CONFIG.methods.email.fromField,
        emailVerification: {
          getEmailContentFn:
            AUTH.CONFIG.methods.email.emailVerification.getEmailContentFn,
          clientRoute: 'undefined',
        },
        passwordReset: {
          getEmailContentFn:
            AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn,
          clientRoute: 'undefined',
        },
      }
      const parseRouteRef = makeRefParser('Route', [])

      expect(() => mapEmailAuth(emailAuth, parseRouteRef)).toThrowError(
        `Invalid Route reference: undefined`
      )
    })
  })

  describe('mapEmailVerification', () => {
    test('should map correctly', () => {
      const emailVerification = AUTH.CONFIG.methods.email.emailVerification
      const parseRouteRef = makeRefParser('Route', [
        AUTH.CONFIG.methods.email.emailVerification.clientRoute,
      ])

      const result = mapEmailVerification(emailVerification, parseRouteRef)

      expect(result).toStrictEqual({
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
      } satisfies AppSpec.EmailVerificationConfig)
    })
  })

  describe('mapPasswordReset', () => {
    test('should map correctly', () => {
      const passwordReset = AUTH.CONFIG.methods.email.passwordReset
      const parseRouteRef = makeRefParser('Route', [
        AUTH.CONFIG.methods.email.passwordReset.clientRoute,
      ])

      const result = mapPasswordReset(passwordReset, parseRouteRef)

      expect(result).toStrictEqual({
        getEmailContentFn: {
          kind: 'named',
          name: AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn
            .import,
          path: AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn.from,
        },
        clientRoute: {
          name: AUTH.CONFIG.methods.email.passwordReset.clientRoute,
          declType: 'Route',
        },
      } satisfies AppSpec.PasswordResetConfig)
    })
  })

  describe('mapUsernameAndPassword', () => {
    test('should map correctly', () => {
      const usernameAndPassword = AUTH.CONFIG.methods.usernameAndPassword

      const result = mapUsernameAndPassword(usernameAndPassword)

      expect(result).toStrictEqual({
        userSignupFields: {
          kind: 'named',
          name: AUTH.CONFIG.methods.usernameAndPassword.userSignupFields.import,
          path: AUTH.CONFIG.methods.usernameAndPassword.userSignupFields.from,
        },
      } satisfies AppSpec.UsernameAndPasswordConfig)
    })
  })

  describe('mapExternalAuth', () => {
    test('should map correctly', () => {
      const externalAuth = AUTH.CONFIG.methods.discord

      const result = mapExternalAuth(externalAuth)

      expect(result).toStrictEqual({
        configFn: {
          kind: 'named',
          name: AUTH.CONFIG.methods.discord.configFn.import,
          path: AUTH.CONFIG.methods.discord.configFn.from,
        },
        userSignupFields: {
          kind: 'named',
          name: AUTH.CONFIG.methods.discord.userSignupFields.import,
          path: AUTH.CONFIG.methods.discord.userSignupFields.from,
        },
      } satisfies AppSpec.ExternalAuthConfig)
    })
  })

  describe('mapAuthMethods', () => {
    test('should map minimal config correctly', () => {
      const authMethods: UserSpec.AuthMethods = {
        usernameAndPassword: undefined,
        discord: undefined,
        google: undefined,
        gitHub: undefined,
        keycloak: undefined,
        email: undefined,
      }

      const parseRouteRef = makeRefParser('Route', [])
      const result = mapAuthMethods(authMethods, parseRouteRef)

      expect(result).toStrictEqual({
        usernameAndPassword: undefined,
        discord: undefined,
        google: undefined,
        gitHub: undefined,
        keycloak: undefined,
        email: undefined,
      } satisfies AppSpec.AuthMethods)
    })
  })

  describe('mapDb', () => {
    test('should map minimal config correctly', () => {
      const db: UserSpec.DbConfig = {
        seeds: [],
      }

      const result = mapDb(db)

      expect(result).toStrictEqual({
        seeds: [],
      } satisfies AppSpec.Db)
    })

    test('should map correctly', () => {
      const db: UserSpec.DbConfig = {
        seeds: DB.CONFIG.seeds,
      }

      const result = mapDb(db)

      expect(result).toStrictEqual({
        seeds: DB.CONFIG.seeds.map((seed) => mapExtImport(seed)),
      } satisfies AppSpec.Db)
    })
  })

  describe('mapEmailSender', () => {
    test('should map minimal config correctly', () => {
      const emailSender: UserSpec.EmailSender = {
        provider: EMAIL.CONFIG.provider,
        defaultFrom: EMAIL.CONFIG.defaultFrom,
      }

      const result = mapEmailSender(emailSender)

      expect(result).toStrictEqual({
        provider: EMAIL.CONFIG.provider,
        defaultFrom: EMAIL.CONFIG.defaultFrom,
      } satisfies AppSpec.EmailSender)
    })
  })

  describe('mapServer', () => {
    test('should map minimal config correctly', () => {
      const server: UserSpec.ServerConfig = {
        setupFn: SERVER.CONFIG.setupFn,
        middlewareConfigFn: SERVER.CONFIG.middlewareConfigFn,
        envValidationSchema: SERVER.CONFIG.envValidationSchema,
      }

      const result = mapServer(server)

      expect(result).toStrictEqual({
        setupFn: {
          kind: 'named',
          name: SERVER.CONFIG.setupFn.import,
          path: SERVER.CONFIG.setupFn.from,
        },
        middlewareConfigFn: {
          kind: 'named',
          name: SERVER.CONFIG.middlewareConfigFn.import,
          path: SERVER.CONFIG.middlewareConfigFn.from,
        },
        envValidationSchema: {
          kind: 'named',
          name: SERVER.CONFIG.envValidationSchema.import,
          path: SERVER.CONFIG.envValidationSchema.from,
        },
      } satisfies AppSpec.Server)
    })
  })

  describe('mapWebSocket', () => {
    test('should map minimal config correctly', () => {
      const webSocket: UserSpec.WebsocketConfig = {
        fn: WEBSOCKET.CONFIG.fn,
      }

      const result = mapWebSocket(webSocket)

      expect(result).toStrictEqual({
        fn: {
          kind: 'named',
          name: WEBSOCKET.CONFIG.fn.import,
          path: WEBSOCKET.CONFIG.fn.from,
        },
        autoConnect: undefined,
      } satisfies AppSpec.WebSocket)
    })

    test('should map correctly', () => {
      const result = mapWebSocket(WEBSOCKET.CONFIG)

      expect(result).toStrictEqual({
        fn: {
          kind: 'named',
          name: WEBSOCKET.CONFIG.fn.import,
          path: WEBSOCKET.CONFIG.fn.from,
        },
        autoConnect: WEBSOCKET.CONFIG.autoConnect,
      } satisfies AppSpec.WebSocket)
    })
  })

  describe('mapClient', () => {
    test('should map minimal config correctly', () => {
      const client: UserSpec.ClientConfig = {}

      const result = mapClient(client)

      expect(result).toStrictEqual({
        rootComponent: undefined,
        setupFn: undefined,
        baseDir: undefined,
        envValidationSchema: undefined,
      } satisfies AppSpec.Client)
    })

    test('should map correctly', () => {
      const result = mapClient(CLIENT.CONFIG)

      expect(result).toStrictEqual({
        rootComponent: {
          kind: 'named',
          name: CLIENT.CONFIG.rootComponent.import,
          path: CLIENT.CONFIG.rootComponent.from,
        },
        setupFn: {
          kind: 'named',
          name: CLIENT.CONFIG.setupFn.import,
          path: CLIENT.CONFIG.setupFn.from,
        },
        baseDir: CLIENT.CONFIG.baseDir,
        envValidationSchema: {
          kind: 'named',
          name: CLIENT.CONFIG.envValidationSchema.import,

          path: CLIENT.CONFIG.envValidationSchema.from,
        },
      } satisfies AppSpec.Client)
    })
  })
})
