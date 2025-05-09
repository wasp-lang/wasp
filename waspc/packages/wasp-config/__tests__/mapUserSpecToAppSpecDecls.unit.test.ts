import { describe, expect, test } from 'vitest'
import { GET_USER_SPEC } from '../src/_private.js'
import * as AppSpec from '../src/appSpec.js'
import {
  makeRefParser,
  mapApi,
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
  mapEmailFromField,
  mapEmailSender,
  mapEmailVerification,
  mapExternalAuth,
  mapExtImport,
  mapHttpRoute,
  mapJob,
  mapOperation,
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
    testMapApp(Fixtures.createUserApp('minimal'))
  })

  test('should map full config correctly', () => {
    testMapApp(Fixtures.createUserApp('full'))
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
    testMapAuth(Fixtures.AUTH.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapAuth(Fixtures.AUTH.FULL)
  })

  test('should throw if userEntity is not provided to entity parser', () => {
    testMapAuth(Fixtures.AUTH.MINIMAL, {
      overrideEntities: [],
      shouldError: true,
    })
  })

  test('should throw if externalAuthEntity ref is not provided when defined', () => {
    const auth = Fixtures.AUTH.FULL
    testMapAuth(auth, {
      overrideEntities: [auth.userEntity],
      shouldError: true,
    })
  })

  test('should throw if emailVerification clientRoute ref is not provided when defined', () => {
    const auth = Fixtures.AUTH.FULL
    testMapAuth(auth, {
      overrideRoutes: [auth.methods.email.emailVerification.clientRoute],
      shouldError: true,
    })
  })

  test('should throw if passwordReset clientRoute ref is not provided when defined', () => {
    const auth = Fixtures.AUTH.FULL
    testMapAuth(auth, {
      overrideRoutes: [auth.methods.email.passwordReset.clientRoute],
      shouldError: true,
    })
  })

  function testMapAuth(
    auth: UserApi.AuthConfig,
    options:
      | {
          overrideEntities?: string[]
          overrideRoutes?: string[]
          shouldError: boolean | undefined
        }
      | undefined = {
      overrideEntities: undefined,
      overrideRoutes: undefined,
      shouldError: false,
    }
  ): void {
    const { overrideEntities, overrideRoutes, shouldError } = options
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

    if (shouldError) {
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
    testMapAuthMethods(Fixtures.AUTH_METHODS.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapAuthMethods(Fixtures.AUTH_METHODS.FULL)
  })

  test('should throw if emailVerification clientRoute ref is not provided when defined', () => {
    const authMethods = Fixtures.AUTH_METHODS.FULL
    testMapAuthMethods(authMethods, {
      overrideRoutes: [authMethods.email.emailVerification.clientRoute],
      shouldError: true,
    })
  })

  test('should throw if passwordReset clientRoute ref is not provided when defined', () => {
    const authMethods = Fixtures.AUTH_METHODS.FULL
    testMapAuthMethods(authMethods, {
      overrideRoutes: [authMethods.email.passwordReset.clientRoute],
      shouldError: true,
    })
  })

  function testMapAuthMethods(
    authMethods: UserApi.AuthMethods,
    options:
      | {
          overrideRoutes?: string[]
          shouldError: boolean | undefined
        }
      | undefined = {
      overrideRoutes: undefined,
      shouldError: false,
    }
  ): void {
    const { overrideRoutes, shouldError } = options
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

    if (shouldError) {
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
    testMapEmailAuth(Fixtures.EMAIL_AUTH.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapEmailAuth(Fixtures.EMAIL_AUTH.FULL)
  })

  test('should throw if emailVerification clientRoute ref is not provided when defined', () => {
    const emailAuth = Fixtures.EMAIL_AUTH.FULL
    testMapEmailAuth(emailAuth, {
      overrideRoutes: [emailAuth.passwordReset.clientRoute],
      shouldError: true,
    })
  })

  test('should throw if passwordReset clientRoute ref is not provided when defined', () => {
    const emailAuth = Fixtures.EMAIL_AUTH.FULL
    testMapEmailAuth(emailAuth, {
      overrideRoutes: [emailAuth.emailVerification.clientRoute],
      shouldError: true,
    })
  })

  function testMapEmailAuth(
    emailAuth: UserApi.EmailAuthConfig,
    options:
      | {
          overrideRoutes?: string[]
          shouldError: boolean | undefined
        }
      | undefined = {
      overrideRoutes: undefined,
      shouldError: false,
    }
  ): void {
    const { overrideRoutes, shouldError } = options
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

    if (shouldError) {
      expect(() => mapEmailAuth(emailAuth, routeRefParser)).toThrowError()
      return
    }

    const result = mapEmailAuth(emailAuth, routeRefParser)

    expect(result).toStrictEqual({
      userSignupFields:
        emailAuth.userSignupFields && mapExtImport(emailAuth.userSignupFields),
      fromField: mapEmailFromField(emailAuth.fromField),
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
    testMapEmailVerification(Fixtures.EMAIL_VERIFICATION.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapEmailVerification(Fixtures.EMAIL_VERIFICATION.FULL)
  })

  test('should throw if clientRoute ref is not provided when defined', () => {
    testMapEmailVerification(Fixtures.EMAIL_VERIFICATION.FULL, {
      overrideRoutes: [],
      shouldError: true,
    })
  })

  function testMapEmailVerification(
    emailVerification: UserApi.EmailVerificationConfig,
    options:
      | {
          overrideRoutes?: string[]
          shouldError: boolean | undefined
        }
      | undefined = {
      overrideRoutes: undefined,
      shouldError: false,
    }
  ): void {
    const { overrideRoutes, shouldError } = options
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

    if (shouldError) {
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
    testMapPasswordReset(Fixtures.PASSWORD_RESET.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapPasswordReset(Fixtures.PASSWORD_RESET.FULL)
  })

  test('should throw if clientRoute ref is not provided when defined', () => {
    testMapPasswordReset(Fixtures.PASSWORD_RESET.FULL, {
      overrideRoutes: [],
      shouldError: true,
    })
  })

  function testMapPasswordReset(
    passwordReset: UserApi.PasswordResetConfig,
    options:
      | {
          overrideRoutes?: string[]
          shouldError: boolean | undefined
        }
      | undefined = {
      overrideRoutes: undefined,
      shouldError: false,
    }
  ): void {
    const { overrideRoutes, shouldError } = options
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

    if (shouldError) {
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
    testMapUsernameAndPassword(Fixtures.USERNAME_AND_PASSWORD_AUTH.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapUsernameAndPassword(Fixtures.USERNAME_AND_PASSWORD_AUTH.FULL)
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
    testMapExternalAuth(Fixtures.EXTERNAL_AUTH.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapExternalAuth(Fixtures.EXTERNAL_AUTH.FULL)
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
    testMapClient(Fixtures.CLIENT.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapClient(Fixtures.CLIENT.FULL)
  })

  function testMapClient(client: UserApi.ClientConfig): void {
    const result = mapClient(client)

    expect(result).toStrictEqual({
      rootComponent: client.rootComponent && mapExtImport(client.rootComponent),
      setupFn: client.setupFn && mapExtImport(client.setupFn),
      baseDir: client.baseDir,
      envValidationSchema:
        client.envValidationSchema && mapExtImport(client.envValidationSchema),
    } satisfies AppSpec.Client)
  }
})

describe('mapServer', () => {
  test('should map minimal config correctly', () => {
    testMapServer(Fixtures.SERVER.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapServer(Fixtures.SERVER.FULL)
  })

  function testMapServer(server: UserApi.ServerConfig): void {
    const result = mapServer(server)

    expect(result).toStrictEqual({
      setupFn: server.setupFn && mapExtImport(server.setupFn),
      middlewareConfigFn:
        server.middlewareConfigFn && mapExtImport(server.middlewareConfigFn),
      envValidationSchema:
        server.envValidationSchema && mapExtImport(server.envValidationSchema),
    } satisfies AppSpec.Server)
  }
})

describe('mapEmailSender', () => {
  test('should map minimal config correctly', () => {
    testMapEmailSender(Fixtures.EMAIL_SENDER.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapEmailSender(Fixtures.EMAIL_SENDER.FULL)
  })

  function testMapEmailSender(emailSender: UserApi.EmailSender): void {
    const result = mapEmailSender(emailSender)

    expect(result).toStrictEqual({
      provider: emailSender.provider,
      defaultFrom:
        emailSender.defaultFrom && mapEmailFromField(emailSender.defaultFrom),
    } satisfies AppSpec.EmailSender)
  }
})

describe('mapWebSocket', () => {
  test('should map minimal config correctly', () => {
    testMapWebSocket(Fixtures.WEBSOCKET.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapWebSocket(Fixtures.WEBSOCKET.FULL)
  })

  function testMapWebSocket(websocket: UserApi.WebsocketConfig): void {
    const result = mapWebSocket(websocket)

    expect(result).toStrictEqual({
      fn: mapExtImport(websocket.fn),
      autoConnect: websocket.autoConnect,
    } satisfies AppSpec.WebSocket)
  }
})

describe('mapDb', () => {
  test('should map minimal config correctly', () => {
    testDb(Fixtures.DB.MINIMAL)
  })

  test('should map full config correctly', () => {
    testDb(Fixtures.DB.FULL)
  })

  function testDb(db: UserApi.DbConfig): void {
    const result = mapDb(db)

    expect(result).toStrictEqual({
      seeds: db.seeds?.map((seed) => mapExtImport(seed)),
    } satisfies AppSpec.Db)
  }
})

describe('mapPage', () => {
  test('should map minimal config correctly', () => {
    testMapPage(Fixtures.PAGES.MINIMAL.config)
  })

  test('should map full config correctly', () => {
    testMapPage(Fixtures.PAGES.FULL.config)
  })

  function testMapPage(page: UserApi.PageConfig): void {
    const result = mapPage(page)

    expect(result).toStrictEqual({
      component: mapExtImport(page.component),
      authRequired: page.authRequired,
    } satisfies AppSpec.Page)
  }
})

describe('mapRoute', () => {
  // NOTE: currently minimal config is the same as full config
  test('should map minimal config correctly', () => {
    testMapRoute(Fixtures.ROUTES.MINIMAL.config)
  })

  test('should map full config correctly', () => {
    testMapRoute(Fixtures.ROUTES.FULL.config)
  })

  function testMapRoute(route: UserApi.RouteConfig): void {
    const pageRefParser = makeRefParser('Page', [route.to])

    const result = mapRoute(route, pageRefParser)

    expect(result).toStrictEqual({
      path: route.path,
      to: pageRefParser(route.to),
    } satisfies AppSpec.Route)
  }
})

describe('mapOperation', () => {
  test('should map minimal query config correctly', () => {
    testMapOperation(Fixtures.QUERIES.MINIMAL.config)
  })

  test('should map full query config correctly', () => {
    testMapOperation(Fixtures.QUERIES.FULL.config)
  })

  test('should throw if entity ref is not provided in query config', () => {
    testMapOperation(Fixtures.QUERIES.FULL.config, {
      overrideEntities: [],
      shouldError: true,
    })
  })

  test('should map minimal action config correctly', () => {
    testMapOperation(Fixtures.ACTIONS.MINIMAL.config)
  })

  test('should map action config correctly', () => {
    testMapOperation(Fixtures.ACTIONS.FULL.config)
  })

  test('should throw if entity ref is not provided in action config', () => {
    testMapOperation(Fixtures.ACTIONS.FULL.config, {
      overrideEntities: [],
      shouldError: true,
    })
  })

  function testMapOperation(
    operation: UserApi.ActionConfig | UserApi.QueryConfig,
    options:
      | {
          overrideEntities?: string[]
          shouldError: boolean | undefined
        }
      | undefined = {
      overrideEntities: undefined,
      shouldError: false,
    }
  ): void {
    const { overrideEntities, shouldError } = options
    const entities = overrideEntities ?? []
    if (!overrideEntities) {
      if (operation.entities) {
        entities.push(...operation.entities)
      }
    }
    const entityRefParser = makeRefParser('Entity', entities)

    if (shouldError) {
      expect(() => mapOperation(operation, entityRefParser)).toThrowError()
      return
    }

    const result = mapOperation(operation, entityRefParser)

    expect(result).toStrictEqual({
      fn: mapExtImport(operation.fn),
      entities: operation.entities?.map(entityRefParser),
      auth: operation.auth,
    } satisfies AppSpec.Query)
  }
})

describe('mapCrud', () => {
  test('should map minimal config correctly', () => {
    testMapCrud(Fixtures.CRUDS.MINIMAL.config)
  })

  test('should map full config correctly', () => {
    testMapCrud(Fixtures.CRUDS.FULL.config)
  })

  test('should throw if entity ref is not provided', () => {
    testMapCrud(Fixtures.CRUDS.FULL.config, {
      overrideEntities: [],
      shouldError: true,
    })
  })

  function testMapCrud(
    crud: UserApi.Crud,
    options:
      | {
          overrideEntities?: string[]
          shouldError: boolean | undefined
        }
      | undefined = {
      overrideEntities: undefined,
      shouldError: false,
    }
  ): void {
    const { overrideEntities, shouldError } = options
    const entities = overrideEntities ?? []
    if (!overrideEntities) {
      if (crud.entity) {
        entities.push(crud.entity)
      }
    }
    const entityRefParser = makeRefParser('Entity', entities)

    if (shouldError) {
      expect(() => mapCrud(crud, entityRefParser)).toThrowError()
      return
    }

    const result = mapCrud(crud, entityRefParser)

    expect(result).toStrictEqual({
      entity: entityRefParser(crud.entity),
      operations: mapCrudOperations(crud.operations),
    } satisfies AppSpec.Crud)
  }
})

describe('mapCrudOperations', () => {
  test('should map minimal config correctly', () => {
    testMapCrudOperations(Fixtures.CRUD_OPERATIONS.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapCrudOperations(Fixtures.CRUD_OPERATIONS.FULL)
  })

  function testMapCrudOperations(crudOperations: UserApi.CrudOperations): void {
    const result = mapCrudOperations(crudOperations)

    expect(result).toStrictEqual({
      get: crudOperations.get && mapCrudOperationOptions(crudOperations.get),
      getAll:
        crudOperations.getAll && mapCrudOperationOptions(crudOperations.getAll),
      create:
        crudOperations.create && mapCrudOperationOptions(crudOperations.create),
      update:
        crudOperations.update && mapCrudOperationOptions(crudOperations.update),
      delete:
        crudOperations.delete && mapCrudOperationOptions(crudOperations.delete),
    } satisfies AppSpec.CrudOperations)
  }
})

describe('mapCrudOperationOptions', () => {
  test('should map minimal config correctly', () => {
    testMapCrudOperationOptions(Fixtures.CRUD_OPERATION_OPTIONS.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapCrudOperationOptions(Fixtures.CRUD_OPERATION_OPTIONS.FULL)
  })

  function testMapCrudOperationOptions(
    crudOperationOptions: UserApi.CrudOperationOptions
  ): void {
    const result = mapCrudOperationOptions(crudOperationOptions)

    expect(result).toStrictEqual({
      isPublic: crudOperationOptions.isPublic,
      overrideFn:
        crudOperationOptions.overrideFn &&
        mapExtImport(crudOperationOptions.overrideFn),
    } satisfies AppSpec.CrudOperationOptions)
  }
})

describe('mapApiNamespace', () => {
  // NOTE: currently minimal config is the same as full config
  test('should map minimal config correctly', () => {
    testMapApiNamespace(Fixtures.API_NAMESPACES.MINIMAL.config)
  })

  test('should map full config correctly', () => {
    testMapApiNamespace(Fixtures.API_NAMESPACES.FULL.config)
  })

  function testMapApiNamespace(apiNamespace: UserApi.ApiNamespaceConfig): void {
    const result = mapApiNamespace(apiNamespace)

    expect(result).toStrictEqual({
      middlewareConfigFn: mapExtImport(apiNamespace.middlewareConfigFn),
      path: apiNamespace.path,
    } satisfies AppSpec.ApiNamespace)
  }
})

describe('mapApi', () => {
  test('should map minimal config correctly', () => {
    testMapApi(Fixtures.APIS.MINIMAL.config)
  })

  test('should map full config correctly', () => {
    testMapApi(Fixtures.APIS.FULL.config)
  })

  test('should throw if entities refs are not provided', () => {
    testMapApi(Fixtures.APIS.FULL.config, {
      overrideEntities: [],
      shouldError: true,
    })
  })

  function testMapApi(
    api: UserApi.ApiConfig,
    options:
      | {
          overrideEntities?: string[]
          shouldError: boolean | undefined
        }
      | undefined = {
      overrideEntities: undefined,
      shouldError: false,
    }
  ): void {
    const { overrideEntities, shouldError } = options
    const entities = overrideEntities ?? []
    if (!overrideEntities) {
      if (api.entities) {
        entities.push(...api.entities)
      }
    }
    const entityRefParser = makeRefParser('Entity', entities)

    if (shouldError) {
      expect(() => mapApi(api, entityRefParser)).toThrowError()
      return
    }

    const result = mapApi(api, entityRefParser)

    expect(result).toStrictEqual({
      fn: mapExtImport(api.fn),
      middlewareConfigFn:
        api.middlewareConfigFn && mapExtImport(api.middlewareConfigFn),
      entities: api.entities?.map(entityRefParser),
      httpRoute: mapHttpRoute(api.httpRoute),
      auth: api.auth,
    } satisfies AppSpec.Api)
  }
})

describe('mapHttpRoute', () => {
  // NOTE: currently minimal config is the same as full config
  test('should map minimal config correctly', () => {
    testMapHttpRoute(Fixtures.HTTP_ROUTES.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapHttpRoute(Fixtures.HTTP_ROUTES.FULL)
  })

  function testMapHttpRoute(httpRoute: UserApi.HttpRoute): void {
    const result = mapHttpRoute(httpRoute)

    expect(result).toStrictEqual([
      httpRoute.method,
      httpRoute.route,
    ] satisfies AppSpec.HttpRoute)
  }
})

describe('mapJob', () => {
  test('should map minimal config correctly', () => {
    testMapJob(Fixtures.JOBS.MINIMAL.config)
  })

  test('should map full config correctly', () => {
    testMapJob(Fixtures.JOBS.FULL.config)
  })

  test('should throw if entity ref is not provided', () => {
    testMapJob(Fixtures.JOBS.FULL.config, {
      overrideEntities: [],
      shouldError: true,
    })
  })

  function testMapJob(
    job: UserApi.JobConfig,
    options:
      | {
          overrideEntities?: string[]
          shouldError: boolean | undefined
        }
      | undefined = {
      overrideEntities: undefined,
      shouldError: false,
    }
  ): void {
    const { overrideEntities, shouldError } = options
    const entities = overrideEntities ?? []
    if (!overrideEntities) {
      if (job.entities) {
        entities.push(...job.entities)
      }
    }
    const entityRefParser = makeRefParser('Entity', entities)

    if (shouldError) {
      expect(() => mapJob(job, entityRefParser)).toThrowError()
      return
    }

    const result = mapJob(job, entityRefParser)

    expect(result).toStrictEqual({
      executor: job.executor,
      perform: mapPerform(job.perform),
      schedule: job.schedule && mapSchedule(job.schedule),
      entities: job.entities?.map(entityRefParser),
    } satisfies AppSpec.Job)
  }
})

describe('mapSchedule', () => {
  test('should map minimal config correctly', () => {
    testMapSchedule(Fixtures.SCHEDULE.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapSchedule(Fixtures.SCHEDULE.FULL)
  })

  function testMapSchedule(schedule: UserApi.ScheduleConfig): void {
    const result = mapSchedule(schedule)

    expect(result).toStrictEqual({
      cron: schedule.cron,
      args: schedule.args,
      executorOptions: schedule.executorOptions,
    } satisfies AppSpec.Schedule)
  }
})

describe('mapPerform', () => {
  test('should map minimal config correctly', () => {
    testMapPerform(Fixtures.PERFORM.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapPerform(Fixtures.PERFORM.FULL)
  })

  function testMapPerform(perform: UserApi.Perform): void {
    const result = mapPerform(perform)

    expect(result).toStrictEqual({
      fn: mapExtImport(perform.fn),
      executorOptions: perform.executorOptions,
    } satisfies AppSpec.Perform)
  }
})

describe('mapEmailFromField', () => {
  test('should map minimal config correctly', () => {
    testMapEmailFromField(Fixtures.EMAIL_FROM_FIELD.MINIMAL)
  })

  test('should map full config correctly', () => {
    testMapEmailFromField(Fixtures.EMAIL_FROM_FIELD.FULL)
  })

  function testMapEmailFromField(emailFromField: UserApi.EmailFromField): void {
    const result = mapEmailFromField(emailFromField)

    expect(result).toStrictEqual({
      name: emailFromField.name,
      email: emailFromField.email,
    } satisfies AppSpec.EmailFromField)
  }
})

describe('mapExtImport', () => {
  // NOTE: currently minimal named import is the same as full named import
  test('should map minimal named import correctly', () => {
    testMapExtImport(Fixtures.EXT_IMPORT.MINIMAL.NAMED)
  })

  test('should map full named import correctly', () => {
    testMapExtImport(Fixtures.EXT_IMPORT.FULL.NAMED)
  })

  // NOTE: currently minimal default import is the same as full default import
  test('should map minimal named import correctly', () => {
    testMapExtImport(Fixtures.EXT_IMPORT.MINIMAL.DEFAULT)
  })

  test('should map full named import correctly', () => {
    testMapExtImport(Fixtures.EXT_IMPORT.FULL.DEFAULT)
  })

  test('should throw for missing import kind', () => {
    const extImport: UserApi.ExtImport = {
      from: '@src/myModule',
    } as unknown as UserApi.ExtImport

    testMapExtImport(extImport, {
      shouldError: true,
    })
  })

  // TODO: unskip this test when we deicde how to handle this
  test.skip('should throw for having both import kind', () => {
    const extImport = {
      ...Fixtures.EXT_IMPORT.FULL.NAMED,
      ...Fixtures.EXT_IMPORT.FULL.DEFAULT,
    }

    testMapExtImport(extImport, {
      shouldError: true,
    })
  })

  // TODO: unskip this test when we decide how to handle this
  test.skip('should throw error for invalid from path', () => {
    const extImport: UserApi.ExtImport = {
      import: 'myNamedImport',
      from: './invalid/path',
    } as unknown as UserApi.ExtImport

    testMapExtImport(extImport, {
      shouldError: true,
    })
  })

  function testMapExtImport(
    extImport: UserApi.ExtImport,
    options:
      | {
          shouldError: boolean | undefined
        }
      | undefined = {
      shouldError: false,
    }
  ): void {
    const { shouldError } = options

    if (shouldError) {
      expect(() => mapExtImport(extImport)).toThrowError()
      return
    }

    const result = mapExtImport(extImport)

    if ('import' in extImport) {
      expect(result).toStrictEqual({
        kind: 'named',
        name: extImport.import,
        path: extImport.from,
      } satisfies AppSpec.ExtImport)
    } else if ('importDefault' in extImport) {
      expect(result).toStrictEqual({
        kind: 'default',
        name: extImport.importDefault,
        path: extImport.from,
      } satisfies AppSpec.ExtImport)
    }
  }
})
