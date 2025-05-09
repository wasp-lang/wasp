import { describe, expect, test } from 'vitest'
import { GET_USER_SPEC } from '../src/_private.js'
import * as AppSpec from '../src/appSpec.js'
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
  mapWebSocket,
} from '../src/mapUserSpecToAppSpecDecls.js'
import * as UserApi from '../src/userApi.js'
import * as Fixtures from './testFixtures.js'

describe('mapApp', () => {
  test('should map minimal config correctly', () => {
    const map = Fixtures.createUserApp('minimal')
    testMapApp(map)
  })

  test('should map full config correctly', () => {
    const fullApp = Fixtures.createUserApp('full')
    testMapApp(fullApp)
  })

  function testMapApp(app: UserApi.App): void {
    const userSpec = app[GET_USER_SPEC]()
    const entities: string[] = []
    if (userSpec.auth) {
      if (userSpec.auth.userEntity) {
        entities.push(userSpec.auth.userEntity)
      }
      if (userSpec.auth.externalAuthEntity) {
        entities.push(userSpec.auth.externalAuthEntity)
      }
    }
    const routes: string[] = []
    if (userSpec.auth) {
      if (userSpec.auth.methods.email?.emailVerification.clientRoute) {
        routes.push(userSpec.auth.methods.email.emailVerification.clientRoute)
      }
      if (userSpec.auth.methods.email?.passwordReset.clientRoute) {
        routes.push(userSpec.auth.methods.email.passwordReset.clientRoute)
      }
    }
    const entityRefParser = makeRefParser('Entity', entities)
    const routeRefParser = makeRefParser('Route', routes)

    const result = mapApp(
      userSpec.app.config,
      entityRefParser,
      routeRefParser,
      userSpec.auth,
      userSpec.server,
      userSpec.client,
      userSpec.db,
      userSpec.emailSender,
      userSpec.websocket
    )

    expect(result).toStrictEqual({
      wasp: {
        version: userSpec.app.config.wasp.version,
      },
      title: userSpec.app.config.title,
      head: userSpec.app.config.head,
      auth:
        userSpec.auth &&
        mapAuth(userSpec.auth, entityRefParser, routeRefParser),
      server: userSpec.server && mapServer(userSpec.server),
      client: userSpec.client && mapClient(userSpec.client),
      db: userSpec.db && mapDb(userSpec.db),
      emailSender: userSpec.emailSender && mapEmailSender(userSpec.emailSender),
      webSocket: userSpec.websocket && mapWebSocket(userSpec.websocket),
    } satisfies AppSpec.App)
  }
})

describe('mapAuth', () => {
  test('should map minimal config correctly', () => {
    const auth = Fixtures.AUTH.MINIMAL.CONFIG
    testMapAuth(auth)
  })

  test('should map full config correctly', () => {
    const auth = Fixtures.AUTH.FULL.CONFIG
    testMapAuth(auth)
  })

  test('should throw if userEntity is not provided to entity parser', () => {
    const auth = Fixtures.AUTH.MINIMAL.CONFIG
    testMapAuth(auth, {
      overrideEntities: [],
      error: true,
    })
  })

  test('should throw if externalAuthEntity ref is not provided when defined', () => {
    const auth = Fixtures.AUTH.FULL.CONFIG
    testMapAuth(auth, {
      overrideEntities: [auth.userEntity],
      error: true,
    })
  })

  test('should throw if emailVerification clientRoute ref is not provided when defined', () => {
    const auth = Fixtures.AUTH.FULL.CONFIG
    testMapAuth(auth, {
      overrideRoutes: [auth.methods.email.emailVerification.clientRoute],
      error: true,
    })
  })

  test('should throw if passwordReset clientRoute ref is not provided when defined', () => {
    const auth = Fixtures.AUTH.FULL.CONFIG
    testMapAuth(auth, {
      overrideRoutes: [auth.methods.email.passwordReset.clientRoute],
      error: true,
    })
  })

  function testMapAuth(
    auth: UserApi.AuthConfig,
    options:
      | {
          overrideEntities?: string[]
          overrideRoutes?: string[]
          error: boolean | undefined
        }
      | undefined = {
      overrideEntities: undefined,
      overrideRoutes: undefined,
      error: false,
    }
  ): void {
    const { overrideEntities, overrideRoutes, error } = options
    const entities = overrideEntities ?? []
    if (!overrideEntities) {
      if (auth.userEntity) {
        entities.push(auth.userEntity)
      }
      if (auth.externalAuthEntity) {
        entities.push(auth.externalAuthEntity)
      }
    }

    const routes = overrideRoutes ?? []
    if (!overrideRoutes) {
      if (auth.methods.email?.emailVerification.clientRoute) {
        routes.push(auth.methods.email.emailVerification.clientRoute)
      }
      if (auth.methods.email?.passwordReset.clientRoute) {
        routes.push(auth.methods.email.passwordReset.clientRoute)
      }
    }

    const entityRefParser = makeRefParser('Entity', entities)
    const routeRefParser = makeRefParser('Route', routes)

    if (error) {
      expect(() =>
        mapAuth(auth, entityRefParser, routeRefParser)
      ).toThrowError()
      return
    }

    const result = mapAuth(auth, entityRefParser, routeRefParser)

    expect(result).toStrictEqual({
      userEntity: entityRefParser(auth.userEntity),
      externalAuthEntity:
        auth.externalAuthEntity === undefined
          ? undefined
          : entityRefParser(auth.externalAuthEntity),
      methods: mapAuthMethods(auth.methods, routeRefParser),
      onAuthFailedRedirectTo: auth.onAuthFailedRedirectTo,
      onAuthSucceededRedirectTo: auth.onAuthSucceededRedirectTo,
      onBeforeSignup: auth.onBeforeSignup && mapExtImport(auth.onBeforeSignup),
      onAfterSignup: auth.onAfterSignup && mapExtImport(auth.onAfterSignup),
      onBeforeOAuthRedirect:
        auth.onBeforeOAuthRedirect && mapExtImport(auth.onBeforeOAuthRedirect),
      onBeforeLogin: auth.onBeforeLogin && mapExtImport(auth.onBeforeLogin),
      onAfterLogin: auth.onAfterLogin && mapExtImport(auth.onAfterLogin),
    } satisfies AppSpec.Auth)
  }
})

describe('mapAuthMethods', () => {
  test('should map minimal config correctly', () => {
    const authMethods = Fixtures.AUTH_METHODS.MINIMAL.CONFIG
    testMapAuthMethods(authMethods)
  })

  test('should map full config correctly', () => {
    const authMethods = Fixtures.AUTH_METHODS.FULL.CONFIG
    testMapAuthMethods(authMethods)
  })

  test('should throw if emailVerification clientRoute ref is not provided when defined', () => {
    const authMethods = Fixtures.AUTH_METHODS.FULL.CONFIG
    testMapAuthMethods(authMethods, {
      overrideRoutes: [authMethods.email.emailVerification.clientRoute],
      error: true,
    })
  })

  test('should throw if passwordReset clientRoute ref is not provided when defined', () => {
    const authMethods = Fixtures.AUTH_METHODS.FULL.CONFIG
    testMapAuthMethods(authMethods, {
      overrideRoutes: [authMethods.email.passwordReset.clientRoute],
      error: true,
    })
  })

  function testMapAuthMethods(
    authMethods: UserApi.AuthMethods,
    options:
      | {
          overrideRoutes?: string[]
          error: boolean | undefined
        }
      | undefined = {
      overrideRoutes: undefined,
      error: false,
    }
  ): void {
    const { overrideRoutes, error } = options
    const routes = overrideRoutes ?? []
    if (!overrideRoutes) {
      if (authMethods.email?.emailVerification.clientRoute) {
        routes.push(authMethods.email.emailVerification.clientRoute)
      }
      if (authMethods.email?.passwordReset.clientRoute) {
        routes.push(authMethods.email.passwordReset.clientRoute)
      }
    }
    const routeRefParser = makeRefParser('Route', routes)

    if (error) {
      expect(() => mapAuthMethods(authMethods, routeRefParser)).toThrowError()
      return
    }

    const result = mapAuthMethods(authMethods, routeRefParser)

    expect(result).toStrictEqual({
      usernameAndPassword:
        authMethods.usernameAndPassword &&
        mapUsernameAndPassword(authMethods.usernameAndPassword),
      discord: authMethods.discord && mapExternalAuth(authMethods.discord),
      google: authMethods.google && mapExternalAuth(authMethods.google),
      gitHub: authMethods.gitHub && mapExternalAuth(authMethods.gitHub),
      keycloak: authMethods.keycloak && mapExternalAuth(authMethods.keycloak),
      email:
        authMethods.email && mapEmailAuth(authMethods.email, routeRefParser),
    } satisfies AppSpec.AuthMethods)
  }
})

describe('mapEmailAuth', () => {
  test('should map minimal config correctly', () => {
    const emailAuth = Fixtures.EMAIL_AUTH.MINIMAL.CONFIG
    testMapEmailAuth(emailAuth)
  })

  test('should map full config correctly', () => {
    const emailAuth = Fixtures.EMAIL_AUTH.FULL.CONFIG
    testMapEmailAuth(emailAuth)
  })

  test('should throw if emailVerification clientRoute ref is not provided when defined', () => {
    const emailAuth = Fixtures.EMAIL_AUTH.FULL.CONFIG
    testMapEmailAuth(emailAuth, {
      overrideRoutes: [emailAuth.passwordReset.clientRoute],
      error: true,
    })
  })

  test('should throw if passwordReset clientRoute ref is not provided when defined', () => {
    const emailAuth = Fixtures.EMAIL_AUTH.FULL.CONFIG
    testMapEmailAuth(emailAuth, {
      overrideRoutes: [emailAuth.emailVerification.clientRoute],
      error: true,
    })
  })

  function testMapEmailAuth(
    emailAuth: UserApi.EmailAuthConfig,
    options:
      | {
          overrideRoutes?: string[]
          error: boolean | undefined
        }
      | undefined = {
      overrideRoutes: undefined,
      error: false,
    }
  ): void {
    const { overrideRoutes, error } = options
    const routes = overrideRoutes ?? []
    if (!overrideRoutes) {
      if (emailAuth?.emailVerification.clientRoute) {
        routes.push(emailAuth.emailVerification.clientRoute)
      }
      if (emailAuth?.passwordReset.clientRoute) {
        routes.push(emailAuth.passwordReset.clientRoute)
      }
    }
    const routeRefParser = makeRefParser('Route', routes)

    if (error) {
      expect(() => mapEmailAuth(emailAuth, routeRefParser)).toThrowError()
      return
    }

    const result = mapEmailAuth(emailAuth, routeRefParser)

    expect(result).toStrictEqual({
      userSignupFields:
        emailAuth.userSignupFields && mapExtImport(emailAuth.userSignupFields),
      fromField: {
        name: emailAuth.fromField.name,
        email: emailAuth.fromField.email,
      },
      emailVerification: mapEmailVerification(
        emailAuth.emailVerification,
        routeRefParser
      ),
      passwordReset: mapPasswordReset(emailAuth.passwordReset, routeRefParser),
    } satisfies AppSpec.EmailAuthConfig)
  }
})

describe('mapEmailVerification', () => {
  test('should map minimal config correctly', () => {
    const emailVerification = Fixtures.EMAIL_VERIFICATION.MINIMAL.CONFIG
    testMapEmailVerification(emailVerification)
  })

  test('should map full config correctly', () => {
    const emailVerification = Fixtures.EMAIL_VERIFICATION.FULL.CONFIG
    testMapEmailVerification(emailVerification)
  })

  test('should throw if clientRoute ref is not provided when defined', () => {
    const emailVerification = Fixtures.EMAIL_VERIFICATION.FULL.CONFIG
    testMapEmailVerification(emailVerification, {
      overrideRoutes: [],
      error: true,
    })
  })

  function testMapEmailVerification(
    emailVerification: UserApi.EmailVerificationConfig,
    options:
      | {
          overrideRoutes?: string[]
          error: boolean | undefined
        }
      | undefined = {
      overrideRoutes: undefined,
      error: false,
    }
  ): void {
    const { overrideRoutes, error } = options
    const routes = overrideRoutes ?? []
    if (!overrideRoutes) {
      if (emailVerification.clientRoute) {
        routes.push(emailVerification.clientRoute)
      }
      if (emailVerification.clientRoute) {
        routes.push(emailVerification.clientRoute)
      }
    }
    const routeRefParser = makeRefParser('Route', routes)

    if (error) {
      expect(() =>
        mapEmailVerification(emailVerification, routeRefParser)
      ).toThrowError()
      return
    }

    const result = mapEmailVerification(emailVerification, routeRefParser)

    expect(result).toStrictEqual({
      clientRoute: routeRefParser(emailVerification.clientRoute),
      getEmailContentFn:
        emailVerification.getEmailContentFn &&
        mapExtImport(emailVerification.getEmailContentFn),
    } satisfies AppSpec.EmailVerificationConfig)
  }
})

describe('mapPasswordReset', () => {
  test('should map minimal config correctly', () => {
    const passwordReset = Fixtures.PASSWORD_RESET.MINIMAL.CONFIG
    testMapPasswordReset(passwordReset)
  })

  test('should map full config correctly', () => {
    const passwordReset = Fixtures.PASSWORD_RESET.FULL.CONFIG
    testMapPasswordReset(passwordReset)
  })

  test('should throw if clientRoute ref is not provided when defined', () => {
    const passwordReset = Fixtures.PASSWORD_RESET.FULL.CONFIG
    testMapPasswordReset(passwordReset, {
      overrideRoutes: [],
      error: true,
    })
  })

  function testMapPasswordReset(
    passwordReset: UserApi.PasswordResetConfig,
    options:
      | {
          overrideRoutes?: string[]
          error: boolean | undefined
        }
      | undefined = {
      overrideRoutes: undefined,
      error: false,
    }
  ): void {
    const { overrideRoutes, error } = options
    const routes = overrideRoutes ?? []
    if (!overrideRoutes) {
      if (passwordReset.clientRoute) {
        routes.push(passwordReset.clientRoute)
      }
      if (passwordReset.clientRoute) {
        routes.push(passwordReset.clientRoute)
      }
    }
    const routeRefParser = makeRefParser('Route', routes)

    if (error) {
      expect(() =>
        mapPasswordReset(passwordReset, routeRefParser)
      ).toThrowError()
      return
    }

    const result = mapPasswordReset(passwordReset, routeRefParser)

    expect(result).toStrictEqual({
      clientRoute: routeRefParser(passwordReset.clientRoute),
      getEmailContentFn:
        passwordReset.getEmailContentFn &&
        mapExtImport(passwordReset.getEmailContentFn),
    } satisfies AppSpec.PasswordResetConfig)
  }
})

describe('mapUsernameAndPassword', () => {
  test('should map minimal config correctly', () => {
    const usernameAndPassword =
      Fixtures.USERNAME_AND_PASSWORD_AUTH.MINIMAL.CONFIG
    testMapUsernameAndPassword(usernameAndPassword)
  })

  test('should map full config correctly', () => {
    const usernameAndPassword = Fixtures.USERNAME_AND_PASSWORD_AUTH.FULL.CONFIG
    testMapUsernameAndPassword(usernameAndPassword)
  })

  function testMapUsernameAndPassword(
    usernameAndPassword: UserApi.UsernameAndPasswordConfig
  ): void {
    const result = mapUsernameAndPassword(usernameAndPassword)

    expect(result).toStrictEqual({
      userSignupFields:
        usernameAndPassword.userSignupFields &&
        mapExtImport(usernameAndPassword.userSignupFields),
    } satisfies AppSpec.UsernameAndPasswordConfig)
  }
})

describe('mapExternalAuth', () => {
  test('should map minimal config correctly', () => {
    const externalAuth = Fixtures.EXTERNAL_AUTH.MINIMAL.CONFIG
    testMapExternalAuth(externalAuth)
  })

  test('should map full config correctly', () => {
    const externalAuth = Fixtures.EXTERNAL_AUTH.FULL.CONFIG
    testMapExternalAuth(externalAuth)
  })

  function testMapExternalAuth(externalAuth: UserApi.ExternalAuthConfig): void {
    const result = mapExternalAuth(externalAuth)

    expect(result).toStrictEqual({
      configFn: externalAuth.configFn && mapExtImport(externalAuth.configFn),
      userSignupFields:
        externalAuth.userSignupFields &&
        mapExtImport(externalAuth.userSignupFields),
    } satisfies AppSpec.ExternalAuthConfig)
  }
})

describe('mapClient', () => {
  test('should map minimal config correctly', () => {
    const client: UserApi.ClientConfig = {}

    const result = mapClient(client)

    expect(result).toStrictEqual({
      rootComponent: undefined,
      setupFn: undefined,
      baseDir: undefined,
      envValidationSchema: undefined,
    } satisfies AppSpec.Client)
  })

  test('should map full config correctly', () => {
    const client = Fixtures.CLIENT.CONFIG

    const result = mapClient(client)

    expect(result).toStrictEqual({
      rootComponent: mapExtImport(client.rootComponent),
      setupFn: mapExtImport(client.setupFn),
      baseDir: client.baseDir,
      envValidationSchema: mapExtImport(client.envValidationSchema),
    } satisfies AppSpec.Client)
  })
})

describe('mapServer', () => {
  test('should map minimal config correctly', () => {
    const server: UserApi.ServerConfig = {}

    const result = mapServer(server)

    expect(result).toStrictEqual({
      setupFn: undefined,
      middlewareConfigFn: undefined,
      envValidationSchema: undefined,
    } satisfies AppSpec.Server)
  })

  test('should map full config correctly', () => {
    const server = Fixtures.SERVER.CONFIG

    const result = mapServer(server)

    expect(result).toStrictEqual({
      setupFn: mapExtImport(server.setupFn),
      middlewareConfigFn: mapExtImport(server.middlewareConfigFn),
      envValidationSchema: mapExtImport(server.envValidationSchema),
    } satisfies AppSpec.Server)
  })
})

describe('mapEmailSender', () => {
  test('should map minimal config correctly', () => {
    const emailSender: UserApi.EmailSender = {
      provider: Fixtures.EMAIL_SENDER.CONFIG.provider,
    }

    const result = mapEmailSender(emailSender)

    expect(result).toStrictEqual({
      provider: emailSender.provider,
      defaultFrom: undefined,
    } satisfies AppSpec.EmailSender)
  })

  test('should map full config correctly', () => {
    const emailSender = Fixtures.EMAIL_SENDER.CONFIG

    const result = mapEmailSender(emailSender)

    expect(result).toStrictEqual({
      provider: emailSender.provider,
      defaultFrom: emailSender.defaultFrom,
    } satisfies AppSpec.EmailSender)
  })
})

describe('mapWebSocket', () => {
  test('should map minimal config correctly', () => {
    const websocket: UserApi.WebsocketConfig = {
      fn: Fixtures.WEBSOCKET.CONFIG.fn,
    }

    const result = mapWebSocket(websocket)

    expect(result).toStrictEqual({
      fn: mapExtImport(websocket.fn),
      autoConnect: undefined,
    } satisfies AppSpec.WebSocket)
  })

  test('should map full config correctly', () => {
    const websocket = Fixtures.WEBSOCKET.CONFIG

    const result = mapWebSocket(websocket)

    expect(result).toStrictEqual({
      fn: mapExtImport(websocket.fn),
      autoConnect: websocket.autoConnect,
    } satisfies AppSpec.WebSocket)
  })
})

describe('mapDb', () => {
  test('should map minimal config correctly', () => {
    const db: UserApi.DbConfig = {}

    const result = mapDb(db)

    expect(result).toStrictEqual({
      seeds: undefined,
    } satisfies AppSpec.Db)
  })

  test('should map full config correctly', () => {
    const db: UserApi.DbConfig = {
      seeds: Fixtures.DB.CONFIG.seeds,
    }

    const result = mapDb(db)

    expect(result).toStrictEqual({
      seeds: db.seeds?.map((seed) => mapExtImport(seed)),
    } satisfies AppSpec.Db)
  })
})

describe('mapPage', () => {
  test('should map minimal config correctly', () => {
    const page = Fixtures.PAGES.MINIMAL.CONFIG

    const result = mapPage(page)

    expect(result).toStrictEqual({
      component: mapExtImport(page.component),
      authRequired: undefined,
    } satisfies AppSpec.Page)
  })

  test('should map full config correctly', () => {
    const page = Fixtures.PAGES.FULL.CONFIG

    const result = mapPage(page)

    expect(result).toStrictEqual({
      component: mapExtImport(page.component),
      authRequired: page.authRequired,
    } satisfies AppSpec.Page)
  })
})

describe('mapRoute', () => {
  test('should map full config correctly', () => {
    const route = Fixtures.ROUTES.FULL.CONFIG
    const pageRefParser = makeRefParser('Page', [route.to])

    const result = mapRoute(route, pageRefParser)

    expect(result).toStrictEqual({
      path: route.path,
      to: pageRefParser(route.to),
    } satisfies AppSpec.Route)
  })
})

describe('mapOperationConfig', () => {
  test('should map minimal query config correctly', () => {
    const query = Fixtures.QUERIES.MINIMAL.CONFIG
    const entityRefParser = makeRefParser('Entity', [])

    const result = mapOperationConfig(query, entityRefParser)

    expect(result).toStrictEqual({
      fn: mapExtImport(query.fn),
      entities: undefined,
      auth: undefined,
    } satisfies AppSpec.Query)
  })

  test('should map query config correctly', () => {
    const query = Fixtures.QUERIES.FULL.CONFIG
    const entityRefParser = makeRefParser('Entity', query.entities)

    const result = mapOperationConfig(query, entityRefParser)

    expect(result).toStrictEqual({
      fn: mapExtImport(query.fn),
      entities: query.entities.map(entityRefParser),
      auth: query.auth,
    } satisfies AppSpec.Query)
  })

  test('should throw if entity ref is not provided in query config', () => {
    const query: UserApi.QueryConfig = Fixtures.QUERIES.FULL.CONFIG
    const entityRefParser = makeRefParser('Entity', [])

    expect(() => mapOperationConfig(query, entityRefParser)).toThrowError()
  })

  test('should map minimal action config correctly', () => {
    const action = Fixtures.ACTIONS.MINIMAL.CONFIG
    const entityRefParser = makeRefParser('Entity', [])

    const result = mapOperationConfig(action, entityRefParser)

    expect(result).toStrictEqual({
      fn: mapExtImport(action.fn),
      entities: undefined,
      auth: undefined,
    } satisfies AppSpec.Action)
  })

  test('should map action config correctly', () => {
    const action = Fixtures.ACTIONS.FULL.CONFIG
    const entityRefParser = makeRefParser('Entity', action.entities)

    const result = mapOperationConfig(action, entityRefParser)

    expect(result).toStrictEqual({
      fn: mapExtImport(action.fn),
      entities: action.entities.map(entityRefParser),
      auth: action.auth,
    } satisfies AppSpec.Action)
  })

  test('should throw if entity ref is not provided in action config', () => {
    const action = Fixtures.ACTIONS.FULL.CONFIG
    const entityRefParser = makeRefParser('Entity', [])

    expect(() => mapOperationConfig(action, entityRefParser)).toThrowError()
  })
})

describe('mapCrud', () => {
  test('should map minimal config correctly', () => {
    const crud = Fixtures.CRUDS.MINIMAL.CONFIG
    const entityRefParser = makeRefParser('Entity', [crud.entity])

    const result = mapCrud(crud, entityRefParser)

    expect(result).toStrictEqual({
      entity: entityRefParser(crud.entity),
      operations: mapCrudOperations({}),
    } satisfies AppSpec.Crud)
  })

  test('should map full config correctly', () => {
    const crud = Fixtures.CRUDS.FULL.CONFIG
    const entityRefParser = makeRefParser('Entity', [crud.entity])

    const result = mapCrud(crud, entityRefParser)

    expect(result).toStrictEqual({
      entity: entityRefParser(crud.entity),
      operations: mapCrudOperations(crud.operations),
    } satisfies AppSpec.Crud)
  })

  test('should throw if entity ref is not provided', () => {
    const crud: UserApi.Crud = Fixtures.CRUDS.FULL.CONFIG
    const entityRefParser = makeRefParser('Entity', [])

    expect(() => mapCrud(crud, entityRefParser)).toThrowError()
  })
})

describe('mapCrudOperations', () => {
  test('should map minimal config correctly', () => {
    const crudOperations = Fixtures.CRUD_OPERATIONS.MINIMAL.CONFIG

    const result = mapCrudOperations(crudOperations)

    expect(result).toStrictEqual({
      get: undefined,
      getAll: undefined,
      create: undefined,
      update: undefined,
      delete: undefined,
    } satisfies AppSpec.CrudOperations)
  })

  test('should map full config correctly', () => {
    const crudOperations = Fixtures.CRUD_OPERATIONS.FULL.CONFIG

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
    const crudOperationOptions = Fixtures.CRUD_OPERATION_OPTIONS.MINIMAL.CONFIG

    const result = mapCrudOperationOptions(crudOperationOptions)

    expect(result).toStrictEqual({
      isPublic: undefined,
      overrideFn: undefined,
    } satisfies AppSpec.CrudOperationOptions)
  })

  test('should map full config correctly', () => {
    const crudOperationOptions = Fixtures.CRUD_OPERATION_OPTIONS.FULL.CONFIG

    const result = mapCrudOperationOptions(crudOperationOptions)

    expect(result).toStrictEqual({
      isPublic: crudOperationOptions.isPublic,
      overrideFn: mapExtImport(crudOperationOptions.overrideFn),
    } satisfies AppSpec.CrudOperationOptions)
  })
})

describe('mapApiNamespace', () => {
  test('should map full config correctly', () => {
    const apiNamespace = Fixtures.API_NAMESPACES.FULL.CONFIG

    const result = mapApiNamespace(apiNamespace)

    expect(result).toStrictEqual({
      middlewareConfigFn: mapExtImport(apiNamespace.middlewareConfigFn),
      path: apiNamespace.path,
    } satisfies AppSpec.ApiNamespace)
  })
})

describe('mapApiConfig', () => {
  test('should map minimal config correctly', () => {
    const api = Fixtures.APIS.MINIMAL.CONFIG
    const entityRefParser = makeRefParser('Entity', [])

    const result = mapApiConfig(api, entityRefParser)

    expect(result).toStrictEqual({
      fn: mapExtImport(api.fn),
      httpRoute: mapHttpRoute(api.httpRoute),
      auth: undefined,
      entities: undefined,
      middlewareConfigFn: undefined,
    } satisfies AppSpec.Api)
  })

  test('should map full config correctly', () => {
    const api = Fixtures.APIS.FULL.CONFIG
    const entityRefParser = makeRefParser('Entity', api.entities)

    const result = mapApiConfig(api, entityRefParser)

    expect(result).toStrictEqual({
      fn: mapExtImport(api.fn),
      middlewareConfigFn: mapExtImport(api.middlewareConfigFn),
      entities: api.entities.map(entityRefParser),
      httpRoute: mapHttpRoute(api.httpRoute),
      auth: api.auth,
    } satisfies AppSpec.Api)
  })
})

describe('mapHttpRoute', () => {
  test('should map full config correctly', () => {
    const httpRoute = Fixtures.APIS.FULL.CONFIG.httpRoute

    const result = mapHttpRoute(httpRoute)

    expect(result).toStrictEqual([
      httpRoute.method,
      httpRoute.route,
    ] satisfies AppSpec.HttpRoute)
  })
})

describe('mapJob', () => {
  test('should map minimal config correctly', () => {
    const job = Fixtures.JOBS.MINIMAL.CONFIG
    const entityRefParser = makeRefParser('Entity', [])

    const result = mapJob(job, entityRefParser)

    expect(result).toStrictEqual({
      executor: job.executor,
      perform: mapPerform(job.perform),
      schedule: undefined,
      entities: undefined,
    } satisfies AppSpec.Job)
  })

  test('should map full config correctly', () => {
    const job = Fixtures.JOBS.FULL.CONFIG
    const entityRefParser = makeRefParser('Entity', job.entities)

    const result = mapJob(job, entityRefParser)

    expect(result).toStrictEqual({
      executor: job.executor,
      perform: mapPerform(job.perform),
      schedule: mapSchedule(job.schedule),
      entities: job.entities.map(entityRefParser),
    } satisfies AppSpec.Job)
  })

  test('should throw if entity ref is not provided', () => {
    const job = Fixtures.JOBS.FULL.CONFIG
    const entityRefParser = makeRefParser('Entity', [])

    expect(() => mapJob(job, entityRefParser)).toThrowError()
  })
})

describe('mapSchedule', () => {
  test('should map full config correctly', () => {
    const schedule = Fixtures.JOBS.FULL.CONFIG.schedule

    const result = mapSchedule(schedule)

    expect(result).toStrictEqual({
      cron: schedule.cron,
      args: schedule.args,
      executorOptions: Fixtures.JOBS.FULL.CONFIG.perform.executorOptions,
    } satisfies AppSpec.Schedule)
  })
})

describe('mapPerform', () => {
  test('should map full config correctly', () => {
    const perform = Fixtures.JOBS.FULL.CONFIG.perform

    const result = mapPerform(perform)

    expect(result).toStrictEqual({
      fn: mapExtImport(perform.fn),
      executorOptions: Fixtures.JOBS.FULL.CONFIG.perform.executorOptions,
    } satisfies AppSpec.Perform)
  })
})

describe('mapExtImport', () => {
  test('should map named import correctly', () => {
    const extImport: UserApi.ExtImport = {
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

  test('should map default import correctly', () => {
    const extImport: UserApi.ExtImport = {
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

  test('should throw for missing import kind', () => {
    const extImport: UserApi.ExtImport = {
      from: '@src/myModule',
    } as unknown as UserApi.ExtImport

    expect(() => mapExtImport(extImport)).toThrowError()
  })

  // TODO: unskip this test when we deicde how to handle this
  test.skip('should throw for having both import kind', () => {
    const extImport: UserApi.ExtImport = {
      import: 'myNamedImport',
      from: '@src/myModule',
      importDefault: 'myDefaultImport',
    }

    expect(() => mapExtImport(extImport)).toThrowError()
  })

  // TODO: unskip this test when we deicde how to handle this
  test.skip('should throw error for invalid from path', () => {
    const extImport: UserApi.ExtImport = {
      import: 'myNamedImport',
      from: './invalid/path',
    } as unknown as UserApi.ExtImport

    expect(() => mapExtImport(extImport)).toThrowError()
  })
})
