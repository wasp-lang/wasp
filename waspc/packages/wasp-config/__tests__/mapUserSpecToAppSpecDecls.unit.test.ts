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
    const minimalApp = new UserApi.App(
      Fixtures.APP.MINIMAL.NAME,
      Fixtures.APP.MINIMAL.CONFIG
    )
    const userSpec = minimalApp[GET_USER_SPEC]()
    const parseEntityRef = makeRefParser('Entity', [])
    const parseRouteRef = makeRefParser('Route', [])

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

    expect(result).toBeDefined()
    expect(result).toStrictEqual({
      wasp: {
        version: Fixtures.APP.MINIMAL.CONFIG.wasp.version,
      },
      title: Fixtures.APP.MINIMAL.CONFIG.title,
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
    const app = new UserApi.App(
      Fixtures.APP.FULL.NAME,
      Fixtures.APP.FULL.CONFIG
    )
    app.auth(Fixtures.AUTH.CONFIG)
    app.server(Fixtures.SERVER.CONFIG)
    app.client(Fixtures.CLIENT.CONFIG)
    app.db(Fixtures.DB.CONFIG)
    app.emailSender(Fixtures.EMAIL_SENDER.CONFIG)
    app.webSocket(Fixtures.WEBSOCKET.CONFIG)

    const userSpec = app[GET_USER_SPEC]()
    const parseEntityRef = makeRefParser('Entity', [
      Fixtures.AUTH.CONFIG.userEntity,
      Fixtures.AUTH.CONFIG.externalAuthEntity,
    ])
    const parseRouteRef = makeRefParser('Route', [
      Fixtures.AUTH.CONFIG.methods.email.emailVerification.clientRoute,
      Fixtures.AUTH.CONFIG.methods.email.passwordReset.clientRoute,
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
        version: Fixtures.APP.FULL.CONFIG.wasp.version,
      },
      title: Fixtures.APP.FULL.CONFIG.title,
      head: Fixtures.APP.FULL.CONFIG.head,
      auth:
        userSpec.auth && mapAuth(userSpec.auth, parseEntityRef, parseRouteRef),
      server: userSpec.server && mapServer(userSpec.server),
      client: userSpec.client && mapClient(userSpec.client),
      db: userSpec.db && mapDb(userSpec.db),
      emailSender: userSpec.emailSender && mapEmailSender(userSpec.emailSender),
      webSocket: userSpec.websocket && mapWebSocket(userSpec.websocket),
    } satisfies AppSpec.App)
  })
})

describe('mapAuth', () => {
  test('should map minimal config correctly', () => {
    const minimalAuth: UserApi.AuthConfig = {
      userEntity: Fixtures.AUTH.CONFIG.userEntity,
      methods: {},
      onAuthFailedRedirectTo: Fixtures.AUTH.CONFIG.onAuthFailedRedirectTo,
    }
    const parseEntityRef = makeRefParser('Entity', [minimalAuth.userEntity])
    const parseRouteRef = makeRefParser('Route', [])

    const result = mapAuth(minimalAuth, parseEntityRef, parseRouteRef)

    expect(result).toStrictEqual({
      userEntity: parseEntityRef(Fixtures.AUTH.CONFIG.userEntity),
      externalAuthEntity: undefined,
      methods: mapAuthMethods(minimalAuth.methods, parseRouteRef),
      onAuthFailedRedirectTo: Fixtures.AUTH.CONFIG.onAuthFailedRedirectTo,
      onAuthSucceededRedirectTo: undefined,
      onBeforeSignup: undefined,
      onAfterSignup: undefined,
      onBeforeOAuthRedirect: undefined,
      onBeforeLogin: undefined,
      onAfterLogin: undefined,
    } satisfies AppSpec.Auth)
  })

  test('should map full config correctly', () => {
    const auth = Fixtures.AUTH.CONFIG
    const parseEntityRef = makeRefParser('Entity', [
      auth.userEntity,
      auth.externalAuthEntity,
    ])
    const parseRouteRef = makeRefParser('Route', [
      auth.methods.email.emailVerification.clientRoute,
      auth.methods.email.passwordReset.clientRoute,
    ])

    const result = mapAuth(auth, parseEntityRef, parseRouteRef)

    expect(result).toStrictEqual({
      userEntity: parseEntityRef(auth.userEntity),
      externalAuthEntity: parseEntityRef(auth.externalAuthEntity),
      methods: mapAuthMethods(auth.methods, parseRouteRef),
      onAuthFailedRedirectTo: auth.onAuthFailedRedirectTo,
      onAuthSucceededRedirectTo: auth.onAuthSucceededRedirectTo,
      onBeforeSignup: mapExtImport(auth.onBeforeSignup),
      onAfterSignup: mapExtImport(auth.onAfterSignup),
      onBeforeOAuthRedirect: mapExtImport(auth.onBeforeOAuthRedirect),
      onBeforeLogin: mapExtImport(auth.onBeforeLogin),
      onAfterLogin: mapExtImport(auth.onAfterLogin),
    } satisfies AppSpec.Auth)
  })

  test('should throw if userEntity is not provided', () => {
    const auth: UserApi.AuthConfig = {
      userEntity: Fixtures.AUTH.CONFIG.userEntity,
      methods: {},
      onAuthFailedRedirectTo: Fixtures.AUTH.CONFIG.onAuthFailedRedirectTo,
    }
    const parseEntityRef = makeRefParser('Entity', [])
    const parseRouteRef = makeRefParser('Route', [])

    expect(() => mapAuth(auth, parseEntityRef, parseRouteRef)).toThrowError()
  })

  test('should throw if externalAuthEntity ref is not provided when defined', () => {
    const auth: UserApi.AuthConfig = {
      userEntity: Fixtures.AUTH.CONFIG.userEntity,
      externalAuthEntity: Fixtures.AUTH.CONFIG.externalAuthEntity,
      methods: {},
      onAuthFailedRedirectTo: Fixtures.AUTH.CONFIG.onAuthFailedRedirectTo,
    }
    const parseEntityRef = makeRefParser('Entity', [
      Fixtures.AUTH.CONFIG.userEntity,
    ])
    const parseRouteRef = makeRefParser('Route', [])

    expect(() => mapAuth(auth, parseEntityRef, parseRouteRef)).toThrowError()
  })
})

describe('mapAuthMethods', () => {
  test('should map minimal config correctly', () => {
    const minimalAuthMethods: UserApi.AuthMethods = {}
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
    const authMethods = Fixtures.AUTH.CONFIG.methods
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
    const minimalEmailAuth: UserApi.EmailAuthConfig = {
      fromField: Fixtures.AUTH.CONFIG.methods.email.fromField,
      emailVerification: Fixtures.AUTH.CONFIG.methods.email.emailVerification,
      passwordReset: Fixtures.AUTH.CONFIG.methods.email.passwordReset,
    }
    const parseRouteRef = makeRefParser('Route', [
      minimalEmailAuth.emailVerification.clientRoute,
      minimalEmailAuth.passwordReset.clientRoute,
    ])

    const result = mapEmailAuth(minimalEmailAuth, parseRouteRef)

    expect(result).toStrictEqual({
      fromField: {
        name: minimalEmailAuth.fromField.name,
        email: minimalEmailAuth.fromField.email,
      },
      emailVerification: mapEmailVerification(
        minimalEmailAuth.emailVerification,
        parseRouteRef
      ),
      passwordReset: mapPasswordReset(
        minimalEmailAuth.passwordReset,
        parseRouteRef
      ),
      userSignupFields: undefined,
    } satisfies AppSpec.EmailAuthConfig)
  })

  test('should map full config correctly', () => {
    const emailAuth = Fixtures.AUTH.CONFIG.methods.email
    const parseRouteRef = makeRefParser('Route', [
      emailAuth.emailVerification.clientRoute,
      emailAuth.passwordReset.clientRoute,
    ])

    const result = mapEmailAuth(emailAuth, parseRouteRef)

    expect(result).toStrictEqual({
      userSignupFields: mapExtImport(emailAuth.userSignupFields),
      fromField: {
        name: emailAuth.fromField.name,
        email: emailAuth.fromField.email,
      },
      emailVerification: mapEmailVerification(
        emailAuth.emailVerification,
        parseRouteRef
      ),
      passwordReset: mapPasswordReset(emailAuth.passwordReset, parseRouteRef),
    } satisfies AppSpec.EmailAuthConfig)
  })

  test('should throw if email verification client route is not provided when defined', () => {
    const emailAuth: UserApi.EmailAuthConfig = {
      ...Fixtures.AUTH.CONFIG.methods.email,
      emailVerification: {
        getEmailContentFn:
          Fixtures.AUTH.CONFIG.methods.email.emailVerification
            .getEmailContentFn,
        clientRoute: 'undefined',
      },
    }
    const parseRouteRef = makeRefParser('Route', [
      emailAuth.passwordReset.clientRoute,
    ])

    expect(() => mapEmailAuth(emailAuth, parseRouteRef)).toThrowError()
  })

  test('should throw if password reset client route is not provided when defined', () => {
    const emailAuth: UserApi.EmailAuthConfig = {
      ...Fixtures.AUTH.CONFIG.methods.email,
      passwordReset: {
        getEmailContentFn:
          Fixtures.AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn,
        clientRoute: 'undefined',
      },
    }
    const parseRouteRef = makeRefParser('Route', [
      emailAuth.emailVerification.clientRoute,
    ])

    expect(() => mapEmailAuth(emailAuth, parseRouteRef)).toThrowError()
  })
})

describe('mapEmailVerification', () => {
  test('should map minimal config correctly', () => {
    const minimalEmailVerification: UserApi.EmailVerificationConfig = {
      clientRoute:
        Fixtures.AUTH.CONFIG.methods.email.emailVerification.clientRoute,
    }
    const parseRouteRef = makeRefParser('Route', [
      minimalEmailVerification.clientRoute,
    ])

    const result = mapEmailVerification(minimalEmailVerification, parseRouteRef)

    expect(result).toStrictEqual({
      getEmailContentFn: undefined,
      clientRoute: parseRouteRef(
        Fixtures.AUTH.CONFIG.methods.email.emailVerification.clientRoute
      ),
    } satisfies AppSpec.EmailVerificationConfig)
  })

  test('should map full config correctly', () => {
    const emailVerification =
      Fixtures.AUTH.CONFIG.methods.email.emailVerification
    const parseRouteRef = makeRefParser('Route', [
      emailVerification.clientRoute,
    ])

    const result = mapEmailVerification(emailVerification, parseRouteRef)

    expect(result).toStrictEqual({
      getEmailContentFn: mapExtImport(emailVerification.getEmailContentFn),
      clientRoute: parseRouteRef(emailVerification.clientRoute),
    } satisfies AppSpec.EmailVerificationConfig)
  })
})

describe('mapPasswordReset', () => {
  test('should map minimal config correctly', () => {
    const minimalPasswordReset: UserApi.PasswordResetConfig = {
      clientRoute: Fixtures.AUTH.CONFIG.methods.email.passwordReset.clientRoute,
    }
    const parseRouteRef = makeRefParser('Route', [
      minimalPasswordReset.clientRoute,
    ])

    const result = mapPasswordReset(minimalPasswordReset, parseRouteRef)

    expect(result).toStrictEqual({
      getEmailContentFn: undefined,
      clientRoute: parseRouteRef(
        Fixtures.AUTH.CONFIG.methods.email.passwordReset.clientRoute
      ),
    } satisfies AppSpec.PasswordResetConfig)
  })

  test('should map full config correctly', () => {
    const passwordResetConfig = Fixtures.AUTH.CONFIG.methods.email.passwordReset
    const parseRouteRef = makeRefParser('Route', [
      Fixtures.AUTH.CONFIG.methods.email.passwordReset.clientRoute,
    ])

    const result = mapPasswordReset(passwordResetConfig, parseRouteRef)

    expect(result).toStrictEqual({
      getEmailContentFn: mapExtImport(
        Fixtures.AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn
      ),
      clientRoute: parseRouteRef(
        Fixtures.AUTH.CONFIG.methods.email.passwordReset.clientRoute
      ),
    } satisfies AppSpec.PasswordResetConfig)
  })
})

describe('mapUsernameAndPassword', () => {
  test('should map minimal config correctly', () => {
    const minimalUsernameAndPassword: UserApi.UsernameAndPasswordConfig = {}

    const result = mapUsernameAndPassword(minimalUsernameAndPassword)

    expect(result).toStrictEqual({
      userSignupFields: undefined,
    } satisfies AppSpec.UsernameAndPasswordConfig)
  })

  test('should map full config correctly', () => {
    const usernameAndPassword = Fixtures.AUTH.CONFIG.methods.usernameAndPassword

    const result = mapUsernameAndPassword(usernameAndPassword)

    expect(result).toStrictEqual({
      userSignupFields: mapExtImport(usernameAndPassword.userSignupFields),
    } satisfies AppSpec.UsernameAndPasswordConfig)
  })
})

describe('mapExternalAuth', () => {
  test('should map minimal config correctly', () => {
    const minimalExternalAuth: UserApi.ExternalAuthConfig = {}

    const result = mapExternalAuth(minimalExternalAuth)

    expect(result).toStrictEqual({
      configFn: undefined,
      userSignupFields: undefined,
    } satisfies AppSpec.ExternalAuthConfig)
  })

  test('should map full config correctly', () => {
    const externalAuth = Fixtures.AUTH.CONFIG.methods.discord

    const result = mapExternalAuth(externalAuth)

    expect(result).toStrictEqual({
      configFn: mapExtImport(externalAuth.configFn),
      userSignupFields: mapExtImport(externalAuth.userSignupFields),
    } satisfies AppSpec.ExternalAuthConfig)
  })
})

describe('mapClient', () => {
  test('should map minimal config correctly', () => {
    const minimalClient: UserApi.ClientConfig = {}

    const result = mapClient(minimalClient)

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
    const minimalServer: UserApi.ServerConfig = {}

    const result = mapServer(minimalServer)

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
    const minimalEmailSender: UserApi.EmailSender = {
      provider: Fixtures.EMAIL_SENDER.CONFIG.provider,
    }

    const result = mapEmailSender(minimalEmailSender)

    expect(result).toStrictEqual({
      provider: minimalEmailSender.provider,
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
    const minimalWebsocket: UserApi.WebsocketConfig = {
      fn: Fixtures.WEBSOCKET.CONFIG.fn,
    }

    const result = mapWebSocket(minimalWebsocket)

    expect(result).toStrictEqual({
      fn: mapExtImport(minimalWebsocket.fn),
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
    const minimalDb: UserApi.DbConfig = {}

    const result = mapDb(minimalDb)

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
    const minimalPage = Fixtures.PAGES.MINIMAL.CONFIG

    const result = mapPage(minimalPage)

    expect(result).toStrictEqual({
      component: mapExtImport(minimalPage.component),
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
    const parsePageRef = makeRefParser('Page', [route.to])

    const result = mapRoute(route, parsePageRef)

    expect(result).toStrictEqual({
      path: route.path,
      to: parsePageRef(route.to),
    } satisfies AppSpec.Route)
  })
})

describe('mapOperationConfig', () => {
  test('should map minimal query config correctly', () => {
    const minimalQuery = Fixtures.QUERIES.MINIMAL.CONFIG
    const parseEntityRef = makeRefParser('Entity', [])

    const result = mapOperationConfig(minimalQuery, parseEntityRef)

    expect(result).toStrictEqual({
      fn: mapExtImport(minimalQuery.fn),
      entities: undefined,
      auth: undefined,
    } satisfies AppSpec.Query)
  })

  test('should map query config correctly', () => {
    const query = Fixtures.QUERIES.FULL.CONFIG
    const parseEntityRef = makeRefParser('Entity', query.entities)

    const result = mapOperationConfig(query, parseEntityRef)

    expect(result).toStrictEqual({
      fn: mapExtImport(query.fn),
      entities: query.entities.map(parseEntityRef),
      auth: query.auth,
    } satisfies AppSpec.Query)
  })

  test('should throw if entity ref is not provided in query config', () => {
    const query: UserApi.QueryConfig = Fixtures.QUERIES.FULL.CONFIG
    const parseEntityRef = makeRefParser('Entity', [])

    expect(() => mapOperationConfig(query, parseEntityRef)).toThrowError()
  })

  test('should map minimal action config correctly', () => {
    const minimalAction = Fixtures.ACTIONS.MINIMAL.CONFIG
    const parseEntityRef = makeRefParser('Entity', [])

    const result = mapOperationConfig(minimalAction, parseEntityRef)

    expect(result).toStrictEqual({
      fn: mapExtImport(minimalAction.fn),
      entities: undefined,
      auth: undefined,
    } satisfies AppSpec.Action)
  })

  test('should map action config correctly', () => {
    const action = Fixtures.ACTIONS.FULL.CONFIG
    const parseEntityRef = makeRefParser('Entity', action.entities)

    const result = mapOperationConfig(action, parseEntityRef)

    expect(result).toStrictEqual({
      fn: mapExtImport(action.fn),
      entities: action.entities.map(parseEntityRef),
      auth: action.auth,
    } satisfies AppSpec.Action)
  })

  test('should throw if entity ref is not provided in action config', () => {
    const action = Fixtures.ACTIONS.FULL.CONFIG
    const parseEntityRef = makeRefParser('Entity', [])

    expect(() => mapOperationConfig(action, parseEntityRef)).toThrowError()
  })
})

describe('mapCrud', () => {
  test('should map minimal config correctly', () => {
    const minimalCrud = Fixtures.CRUDS.MINIMAL.CONFIG
    const parseEntityRef = makeRefParser('Entity', [minimalCrud.entity])

    const result = mapCrud(minimalCrud, parseEntityRef)

    expect(result).toStrictEqual({
      entity: parseEntityRef(minimalCrud.entity),
      operations: mapCrudOperations({}),
    } satisfies AppSpec.Crud)
  })

  test('should map full config correctly', () => {
    const crud = Fixtures.CRUDS.FULL.CONFIG
    const parseEntityRef = makeRefParser('Entity', [crud.entity])

    const result = mapCrud(crud, parseEntityRef)

    expect(result).toStrictEqual({
      entity: parseEntityRef(crud.entity),
      operations: mapCrudOperations(crud.operations),
    } satisfies AppSpec.Crud)
  })

  test('should throw if entity ref is not provided', () => {
    const crud: UserApi.Crud = Fixtures.CRUDS.FULL.CONFIG
    const parseEntityRef = makeRefParser('Entity', [])

    expect(() => mapCrud(crud, parseEntityRef)).toThrowError()
  })
})

describe('mapCrudOperations', () => {
  test('should map minimal config correctly', () => {
    const minimalCrudOperations = Fixtures.CRUDS.MINIMAL.CONFIG.operations

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
    const crudOperations = Fixtures.CRUDS.FULL.CONFIG.operations

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
    const minimalCrudOperationOptions: UserApi.CrudOperationOptions = {
      isPublic: false,
    }

    const result = mapCrudOperationOptions(minimalCrudOperationOptions)

    expect(result).toStrictEqual({
      isPublic: false,
      overrideFn: undefined,
    } satisfies AppSpec.CrudOperationOptions)
  })

  test('should map full config correctly', () => {
    const crudOperationOptions = Fixtures.CRUDS.FULL.CONFIG.operations.get

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
    const minimalApi = Fixtures.APIS.MINIMAL.CONFIG
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
    const api = Fixtures.APIS.FULL.CONFIG
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
    const minimalJob = Fixtures.JOBS.MINIMAL.CONFIG
    const parseEntityRef = makeRefParser('Entity', [])

    const result = mapJob(minimalJob, parseEntityRef)

    expect(result).toStrictEqual({
      executor: minimalJob.executor,
      perform: mapPerform(minimalJob.perform),
      schedule: undefined,
      entities: undefined,
    } satisfies AppSpec.Job)
  })

  test('should map full config correctly', () => {
    const job = Fixtures.JOBS.FULL.CONFIG
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
    const job = Fixtures.JOBS.FULL.CONFIG
    const parseEntityRef = makeRefParser('Entity', [])

    expect(() => mapJob(job, parseEntityRef)).toThrowError()
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

  // test('should throw for having both import kind', () => {
  //   const extImport: UserSpec.ExtImport = {
  //     import: 'myNamedImport',
  //     from: '@src/myModule',
  //     importDefault: 'myDefaultImport',
  //   }

  //   expect(() => mapExtImport(extImport)).toThrowError()
  // })

  test('should throw for missing import kind', () => {
    const extImport: UserApi.ExtImport = {
      from: '@src/myModule',
    } as unknown as UserApi.ExtImport

    expect(() => mapExtImport(extImport)).toThrowError()
  })

  // test('should throw error for invalid from path', () => {
  //   const extImport: UserSpec.ExtImport = {
  //     import: 'myNamedImport',
  //     from: './invalid/path',
  //   } as unknown as UserSpec.ExtImport

  //   expect(() => mapExtImport(extImport)).toThrowError()
  // })
})
