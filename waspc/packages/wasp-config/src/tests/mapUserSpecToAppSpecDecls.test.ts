import { describe, expect, test } from 'vitest'
import { GET_USER_SPEC } from '../_private.js'
import * as AppSpec from '../appSpec.js'
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
import * as Fixtures from './testFixtures.js'

describe('mapUserSpecToAppSpecDecls', () => {
  function createFullUserSpec(): UserSpec.UserSpec {
    const app = new UserSpec.App(Fixtures.APP.NAME, Fixtures.APP.CONFIG)
    app.auth(Fixtures.AUTH.CONFIG)
    app.client(Fixtures.CLIENT.CONFIG)
    app.server(Fixtures.SERVER.CONFIG)
    app.emailSender(Fixtures.EMAIL_SENDER.CONFIG)
    app.webSocket(Fixtures.WEBSOCKET.CONFIG)
    app.db(Fixtures.DB.CONFIG)
    Object.values(Fixtures.PAGES).forEach(({ NAME, CONFIG }) => {
      app.page(NAME, CONFIG)
    })
    Object.values(Fixtures.ROUTES).forEach(({ NAME, CONFIG }) => {
      app.route(NAME, CONFIG)
    })
    Object.values(Fixtures.QUERIES).forEach(({ NAME, CONFIG }) => {
      app.query(NAME, CONFIG)
    })
    Object.values(Fixtures.ACTIONS).forEach(({ NAME, CONFIG }) => {
      app.action(NAME, CONFIG)
    })
    Object.values(Fixtures.CRUDS).forEach(({ NAME, CONFIG }) => {
      app.crud(NAME, CONFIG)
    })
    Object.values(Fixtures.API_NAMESPACES).forEach(({ NAME, CONFIG }) => {
      app.apiNamespace(NAME, CONFIG)
    })
    Object.values(Fixtures.APIS).forEach(({ NAME, CONFIG }) => {
      app.api(NAME, CONFIG)
    })
    Object.values(Fixtures.JOBS).forEach(({ NAME, CONFIG }) => {
      app.job(NAME, CONFIG)
    })
    return app[GET_USER_SPEC]()
  }

  function getDecl<T extends keyof AppSpec.DeclTypeToValue>(
    decls: AppSpec.Decl[],
    declType: T,
    declName: string | undefined = undefined
  ): AppSpec.GetDeclForType<T> | undefined {
    if (declName === undefined) {
      return decls.find((decl) => decl.declType === declType) as
        | AppSpec.GetDeclForType<T>
        | undefined
    } else {
      return decls.find(
        (decl) => decl.declType === declType && decl.declName === declName
      ) as AppSpec.GetDeclForType<T> | undefined
    }
  }

  // This test deliberately avoids using individual mapping functions and instead uses raw values.
  // This serves as an integration test to ensure the complete UserSpec to AppSpec transformation
  // pipeline works correctly end-to-end, independent of the individual mapping functions tested below.
  test('should map end-to-end without mapping functions correctly', () => {
    const userSpec = createFullUserSpec()
    const result = mapUserSpecToAppSpecDecls(userSpec, Fixtures.ALL_ENTITIES)

    const declTypes = result.map((decl) => decl.declType)
    const declNames = result.map((decl) => decl.declName)

    // AppConfig
    expect(declTypes).toContain('App')
    expect(declNames).toContain(Fixtures.APP.NAME)

    const appDecl = getDecl(result, 'App')
    if (!appDecl) {
      throw new Error('App declaration not found')
    }

    expect(appDecl).toBeDefined()
    expect(appDecl.declValue.title).toBe(Fixtures.APP.CONFIG.title)
    expect(appDecl.declValue.wasp.version).toBe(
      Fixtures.APP.CONFIG.wasp.version
    )
    expect(appDecl.declValue.head).toBeDefined()
    expect(appDecl.declValue.head).toHaveLength(1)
    expect(appDecl.declValue.head?.[0]).toBe(Fixtures.APP.CONFIG.head?.[0])

    // AuthConfig
    const auth = appDecl.declValue.auth

    expect(auth).toBeDefined()
    expect(auth?.userEntity).toStrictEqual({
      name: Fixtures.AUTH.CONFIG.userEntity,
      declType: 'Entity',
    })
    expect(auth?.externalAuthEntity).toStrictEqual({
      name: Fixtures.AUTH.CONFIG.externalAuthEntity,
      declType: 'Entity',
    })

    // Discord
    expect(auth?.methods.discord).toBeDefined()
    expect(auth?.methods.discord?.configFn).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.methods.discord.configFn.import,
      path: Fixtures.AUTH.CONFIG.methods.discord.configFn.from,
    })
    expect(auth?.methods.discord?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.methods.discord.userSignupFields.import,
      path: Fixtures.AUTH.CONFIG.methods.discord.userSignupFields.from,
    })

    // GitHub
    expect(auth?.methods.gitHub).toBeDefined()
    expect(auth?.methods.gitHub?.configFn).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.methods.gitHub.configFn.import,
      path: Fixtures.AUTH.CONFIG.methods.gitHub.configFn.from,
    })
    expect(auth?.methods.gitHub?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.methods.gitHub.userSignupFields.import,
      path: Fixtures.AUTH.CONFIG.methods.gitHub.userSignupFields.from,
    })

    // Keycloak
    expect(auth?.methods.keycloak).toBeDefined()
    expect(auth?.methods.keycloak?.configFn).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.methods.keycloak.configFn.import,
      path: Fixtures.AUTH.CONFIG.methods.keycloak.configFn.from,
    })
    expect(auth?.methods.keycloak?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.methods.keycloak.userSignupFields.import,
      path: Fixtures.AUTH.CONFIG.methods.keycloak.userSignupFields.from,
    })

    // Google
    expect(auth?.methods.google).toBeDefined()
    expect(auth?.methods.google?.configFn).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.methods.google.configFn.import,
      path: Fixtures.AUTH.CONFIG.methods.google.configFn.from,
    })
    expect(auth?.methods.google?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.methods.google.userSignupFields.import,
      path: Fixtures.AUTH.CONFIG.methods.google.userSignupFields.from,
    })

    // Username and Password
    expect(auth?.methods.usernameAndPassword).toBeDefined()
    expect(auth?.methods.usernameAndPassword?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.methods.usernameAndPassword.userSignupFields
        .import,
      path: Fixtures.AUTH.CONFIG.methods.usernameAndPassword.userSignupFields
        .from,
    })

    // Email
    expect(auth?.methods.email).toBeDefined()
    expect(auth?.methods.email?.userSignupFields).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.methods.email.userSignupFields.import,
      path: Fixtures.AUTH.CONFIG.methods.email.userSignupFields.from,
    })
    expect(auth?.methods.email?.fromField).toStrictEqual({
      name: Fixtures.AUTH.CONFIG.methods.email.fromField.name,
      email: Fixtures.AUTH.CONFIG.methods.email.fromField.email,
    })
    expect(auth?.methods.email?.emailVerification).toStrictEqual({
      getEmailContentFn: {
        kind: 'named',
        name: Fixtures.AUTH.CONFIG.methods.email.emailVerification
          .getEmailContentFn.import,
        path: Fixtures.AUTH.CONFIG.methods.email.emailVerification
          .getEmailContentFn.from,
      },
      clientRoute: {
        name: Fixtures.AUTH.CONFIG.methods.email.emailVerification.clientRoute,
        declType: 'Route',
      },
    })
    expect(auth?.methods.email?.passwordReset).toStrictEqual({
      getEmailContentFn: {
        kind: 'named',
        name: Fixtures.AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn
          .import,
        path: Fixtures.AUTH.CONFIG.methods.email.passwordReset.getEmailContentFn
          .from,
      },
      clientRoute: {
        name: Fixtures.AUTH.CONFIG.methods.email.passwordReset.clientRoute,
        declType: 'Route',
      },
    })

    // Auth Hooks
    expect(auth?.onAuthFailedRedirectTo).toBe(
      Fixtures.AUTH.CONFIG.onAuthFailedRedirectTo
    )
    expect(auth?.onAuthSucceededRedirectTo).toBe(
      Fixtures.AUTH.CONFIG.onAuthSucceededRedirectTo
    )
    expect(auth?.onBeforeOAuthRedirect).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.onBeforeOAuthRedirect.import,
      path: Fixtures.AUTH.CONFIG.onBeforeOAuthRedirect.from,
    })
    expect(auth?.onBeforeSignup).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.onBeforeSignup.import,
      path: Fixtures.AUTH.CONFIG.onBeforeSignup.from,
    })
    expect(auth?.onAfterSignup).toStrictEqual({
      kind: 'named',
      name: Fixtures.AUTH.CONFIG.onAfterSignup.import,
      path: Fixtures.AUTH.CONFIG.onAfterSignup.from,
    })

    // ClientConfig
    const client = appDecl.declValue.client

    expect(client).toBeDefined()
    expect(client?.rootComponent).toStrictEqual({
      kind: 'named',
      name: Fixtures.CLIENT.CONFIG.rootComponent.import,
      path: Fixtures.CLIENT.CONFIG.rootComponent.from,
    })
    expect(client?.setupFn).toStrictEqual({
      kind: 'named',
      name: Fixtures.CLIENT.CONFIG.setupFn.import,
      path: Fixtures.CLIENT.CONFIG.setupFn.from,
    })
    expect(client?.baseDir).toBe(Fixtures.CLIENT.CONFIG.baseDir)
    expect(client?.envValidationSchema).toStrictEqual({
      kind: 'named',
      name: Fixtures.CLIENT.CONFIG.envValidationSchema.import,
      path: Fixtures.CLIENT.CONFIG.envValidationSchema.from,
    })

    // ServerConfig
    const server = appDecl.declValue.server

    expect(server).toBeDefined()
    expect(server?.setupFn).toStrictEqual({
      kind: 'named',
      name: Fixtures.SERVER.CONFIG.setupFn.import,
      path: Fixtures.SERVER.CONFIG.setupFn.from,
    })
    expect(server?.middlewareConfigFn).toStrictEqual({
      kind: 'named',
      name: Fixtures.SERVER.CONFIG.middlewareConfigFn.import,
      path: Fixtures.SERVER.CONFIG.middlewareConfigFn.from,
    })
    expect(server?.envValidationSchema).toStrictEqual({
      kind: 'named',
      name: Fixtures.SERVER.CONFIG.envValidationSchema.import,
      path: Fixtures.SERVER.CONFIG.envValidationSchema.from,
    })

    // EmailSenderConfig
    const emailSender = appDecl.declValue.emailSender

    expect(emailSender).toBeDefined()
    expect(emailSender?.provider).toBe(Fixtures.EMAIL_SENDER.CONFIG.provider)
    expect(emailSender?.defaultFrom?.email).toBe(
      Fixtures.EMAIL_SENDER.CONFIG.defaultFrom.email
    )
    expect(emailSender?.defaultFrom?.name).toBe(
      Fixtures.EMAIL_SENDER.CONFIG.defaultFrom.name
    )

    // WebSocketConfig
    const webSocket = appDecl.declValue.webSocket

    expect(webSocket).toBeDefined()
    expect(webSocket?.fn).toStrictEqual({
      kind: 'named',
      name: Fixtures.WEBSOCKET.CONFIG.fn.import,
      path: Fixtures.WEBSOCKET.CONFIG.fn.from,
    })

    // DbConfig
    const db = appDecl.declValue.db

    expect(db).toBeDefined()
    expect(db?.seeds).toHaveLength(1)
    expect(db?.seeds?.[0]).toStrictEqual({
      kind: 'named',
      name: Fixtures.DB.CONFIG.seeds[0]?.import,
      path: Fixtures.DB.CONFIG.seeds[0]?.from,
    })

    // PageConfig
    expect(declTypes).toContain('Page')
    Object.values(Fixtures.PAGES).forEach(({ NAME, CONFIG }) => {
      expect(declNames).toContain(NAME)

      const loginPageDecl = getDecl(result, 'Page', NAME)
      if (!loginPageDecl) {
        throw new Error(`Login page declaration for ${NAME} not found`)
      }

      expect(loginPageDecl.declValue.component).toStrictEqual({
        kind: 'named',
        name: CONFIG.component.import,
        path: CONFIG.component.from,
      })
      if ('authRequired' in CONFIG) {
        expect(loginPageDecl.declValue.authRequired).toBe(CONFIG.authRequired)
      }
    })

    // RouteConfig
    expect(declTypes).toContain('Route')
    Object.values(Fixtures.ROUTES).forEach(({ NAME, CONFIG }) => {
      expect(declNames).toContain(NAME)

      const routeDecl = getDecl(result, 'Route', NAME)
      if (!routeDecl) {
        throw new Error(`Route declaration for ${NAME} not found`)
      }

      expect(routeDecl.declValue.path).toBe(CONFIG.path)
      expect(routeDecl.declValue.to).toStrictEqual({
        name: CONFIG.to,
        declType: 'Page',
      })
    })

    // QueryConfig
    expect(declTypes).toContain('Query')
    Object.values(Fixtures.QUERIES).forEach(({ NAME, CONFIG }) => {
      expect(declNames).toContain(NAME)

      const queryDecl = getDecl(result, 'Query', NAME)
      if (!queryDecl) {
        throw new Error(`Query declaration for ${NAME} not found`)
      }

      expect(queryDecl.declValue.fn).toStrictEqual({
        kind: 'named',
        name: CONFIG.fn.import,
        path: CONFIG.fn.from,
      })

      if ('auth' in CONFIG) {
        expect(queryDecl.declValue.auth).toBe(CONFIG.auth)
      }

      if ('entities' in CONFIG) {
        queryDecl.declValue.entities?.forEach((entity, index) => {
          expect(entity).toStrictEqual({
            name: CONFIG.entities[index],
            declType: 'Entity',
          })
        })
      }
    })

    // ActionConfig
    expect(declTypes).toContain('Action')
    Object.values(Fixtures.ACTIONS).forEach(({ NAME, CONFIG }) => {
      expect(declNames).toContain(NAME)

      const actionDecl = getDecl(result, 'Action', NAME)
      if (!actionDecl) {
        throw new Error(`Action declaration for ${NAME} not found`)
      }

      expect(actionDecl.declValue.fn).toStrictEqual({
        kind: 'named',
        name: CONFIG.fn.import,
        path: CONFIG.fn.from,
      })

      if ('auth' in CONFIG) {
        expect(actionDecl.declValue.auth).toBe(CONFIG.auth)
      }

      if ('entities' in CONFIG) {
        expect(actionDecl.declValue.entities).toBeDefined()
        expect(actionDecl.declValue.entities).toHaveLength(
          CONFIG.entities?.length
        )
      }
    })

    // CrudConfig
    expect(declTypes).toContain('Crud')
    Object.values(Fixtures.CRUDS).forEach(({ NAME, CONFIG }) => {
      expect(declNames).toContain(NAME)

      const crudDecl = getDecl(result, 'Crud', NAME)
      if (!crudDecl) {
        throw new Error(`Crud declaration for ${NAME} not found`)
      }

      expect(crudDecl.declValue.entity).toStrictEqual({
        name: CONFIG.entity,
        declType: 'Entity',
      })

      if ('get' in CONFIG.operations) {
        expect(crudDecl.declValue.operations.get?.isPublic).toBe(
          CONFIG.operations?.get.isPublic
        )
        expect(crudDecl.declValue.operations.get?.overrideFn).toStrictEqual({
          kind: 'named',
          name: CONFIG.operations.get.overrideFn.import,
          path: CONFIG.operations.get.overrideFn.from,
        })
      }

      if ('getAll' in CONFIG.operations) {
        expect(crudDecl.declValue.operations.getAll?.isPublic).toBe(
          CONFIG.operations?.getAll.isPublic
        )
        expect(crudDecl.declValue.operations.getAll?.overrideFn).toStrictEqual({
          kind: 'named',
          name: CONFIG.operations.getAll.overrideFn.import,
          path: CONFIG.operations.getAll.overrideFn.from,
        })
      }

      if ('create' in CONFIG.operations) {
        expect(crudDecl.declValue.operations.create?.isPublic).toBe(
          CONFIG.operations.create.isPublic
        )
        expect(crudDecl.declValue.operations.create?.overrideFn).toStrictEqual({
          kind: 'named',
          name: CONFIG.operations.create.overrideFn.import,
          path: CONFIG.operations.create.overrideFn.from,
        })
      }

      if ('update' in CONFIG.operations) {
        expect(crudDecl.declValue.operations.update?.isPublic).toBe(
          CONFIG.operations.update.isPublic
        )
        expect(crudDecl.declValue.operations.update?.overrideFn).toStrictEqual({
          kind: 'named',
          name: CONFIG.operations.update.overrideFn.import,
          path: CONFIG.operations.update.overrideFn.from,
        })
      }

      if ('delete' in CONFIG.operations) {
        expect(crudDecl.declValue.operations.delete?.isPublic).toBe(
          CONFIG.operations.delete.isPublic
        )
        expect(crudDecl.declValue.operations.delete?.overrideFn).toStrictEqual({
          kind: 'named',
          name: CONFIG.operations.delete.overrideFn.import,
          path: CONFIG.operations.delete.overrideFn.from,
        })
      }
    })

    // ApiNamespaceConfig
    expect(declTypes).toContain('ApiNamespace')
    Object.values(Fixtures.API_NAMESPACES).forEach(({ NAME, CONFIG }) => {
      expect(declNames).toContain(NAME)

      const apiNamespaceDecl = getDecl(result, 'ApiNamespace', NAME)
      if (!apiNamespaceDecl) {
        throw new Error(`ApiNamespace declaration for ${NAME} not found`)
      }

      expect(apiNamespaceDecl.declValue.path).toBe(CONFIG.path)
      expect(apiNamespaceDecl.declValue.middlewareConfigFn).toStrictEqual({
        kind: 'named',
        name: CONFIG.middlewareConfigFn.import,
        path: CONFIG.middlewareConfigFn.from,
      })
    })

    // ApiConfig
    expect(declTypes).toContain('Api')
    Object.values(Fixtures.APIS).forEach(({ NAME, CONFIG }) => {
      expect(declNames).toContain(NAME)

      const apiDecl = getDecl(result, 'Api', NAME)
      if (!apiDecl) {
        throw new Error(`Api declaration for ${NAME} not found`)
      }

      expect(apiDecl.declValue.fn).toStrictEqual({
        kind: 'named',
        name: CONFIG.fn.import,
        path: CONFIG.fn.from,
      })
      expect(apiDecl.declValue.httpRoute).toStrictEqual([
        CONFIG.httpRoute.method,
        CONFIG.httpRoute.route,
      ])
      if ('auth' in CONFIG) {
        expect(apiDecl.declValue.auth).toBe(CONFIG.auth)
      }
      if ('entities' in CONFIG) {
        expect(apiDecl.declValue.entities).toBeDefined()
        expect(apiDecl.declValue.entities).toHaveLength(CONFIG.entities.length)
        CONFIG.entities.forEach((entity, index) => {
          expect(apiDecl.declValue.entities?.[index]).toStrictEqual({
            name: entity,
            declType: 'Entity',
          })
        })
      }
      if ('middlewareConfigFn' in CONFIG) {
        expect(apiDecl.declValue.middlewareConfigFn).toStrictEqual({
          kind: 'named',
          name: CONFIG.middlewareConfigFn.import,
          path: CONFIG.middlewareConfigFn.from,
        })
      }
    })

    // JobConfig
    expect(declTypes).toContain('Job')
    Object.values(Fixtures.JOBS).forEach(({ NAME, CONFIG }) => {
      expect(declNames).toContain(NAME)

      const jobDecl = getDecl(result, 'Job', NAME)
      if (!jobDecl) {
        throw new Error(`Job declaration for ${NAME} not found`)
      }

      expect(jobDecl.declValue.executor).toBe(CONFIG.executor)
      expect(jobDecl.declValue.perform).toBeDefined()
      expect(jobDecl.declValue.perform.fn).toStrictEqual({
        kind: 'named',
        name: CONFIG.perform.fn.import,
        path: CONFIG.perform.fn.from,
      })
      if ('executorOptions' in CONFIG.perform) {
        expect(jobDecl.declValue.perform.executorOptions).toStrictEqual({
          pgBoss: CONFIG.perform.executorOptions.pgBoss,
        })
      }
      if ('entities' in CONFIG) {
        expect(jobDecl.declValue.entities).toBeDefined()
        expect(jobDecl.declValue.entities).toHaveLength(CONFIG.entities.length)
        CONFIG.entities.forEach((entity, index) => {
          expect(jobDecl.declValue.entities?.[index]).toStrictEqual({
            name: entity,
            declType: 'Entity',
          })
        })
      }
      if ('schedule' in CONFIG) {
        expect(jobDecl.declValue.schedule?.cron).toBe(CONFIG.schedule.cron)
        if ('args' in CONFIG.schedule) {
          expect(jobDecl.declValue.schedule?.args).toBeDefined()
          expect(jobDecl.declValue.schedule?.args).toStrictEqual(
            CONFIG.schedule.args
          )
        }
        if ('executorOptions' in CONFIG.schedule) {
          expect(jobDecl.declValue.schedule?.executorOptions).toBeDefined()
          expect(jobDecl.declValue.schedule?.executorOptions?.pgBoss).toBe(
            CONFIG.schedule.executorOptions.pgBoss
          )
        }
      }
    })
  })

  test('should map end-to-end using mapping functions correctly', () => {
    const userSpec = createFullUserSpec()
    const parseEntityRef = makeRefParser('Entity', Fixtures.ALL_ENTITIES)
    const parseRouteRef = makeRefParser('Route', Fixtures.ALL_ROUTE_NAMES)
    const parsePageRef = makeRefParser('Page', Fixtures.ALL_PAGE_NAMES)

    const result = mapUserSpecToAppSpecDecls(userSpec, Fixtures.ALL_ENTITIES)

    const declTypes = result.map((decl) => decl.declType)
    const declNames = result.map((decl) => decl.declName)

    // AppConfig
    expect(declTypes).toContain('App')
    expect(declNames).toContain(Fixtures.APP.NAME)

    const appDecl = getDecl(result, 'App')
    if (!appDecl) {
      throw new Error('App declaration not found')
    }

    expect(appDecl).toBeDefined()
    expect(appDecl.declValue).toStrictEqual(
      mapApp(
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
    )

    // Pages
    expect(declTypes).toContain('Page')
    Object.values(Fixtures.PAGES).forEach(({ NAME }) => {
      expect(declNames).toContain(NAME)

      const pageDecl = getDecl(result, 'Page', NAME)
      if (!pageDecl) {
        throw new Error(`Page declaration not found for ${NAME}`)
      }

      const page = userSpec.pages.get(NAME)
      if (!page) {
        throw new Error(`Page config not found for ${NAME}`)
      }
      expect(pageDecl.declValue).toStrictEqual(mapPage(page))
    })

    // Routes
    expect(declTypes).toContain('Route')
    Object.values(Fixtures.ROUTES).forEach(({ NAME }) => {
      expect(declNames).toContain(NAME)

      const routeDecl = getDecl(result, 'Route', NAME)
      if (!routeDecl) {
        throw new Error(`Route declaration not found for ${NAME}`)
      }

      const route = userSpec.routes.get(NAME)
      if (!route) {
        throw new Error(`Route config not found for ${NAME}`)
      }
      expect(routeDecl.declValue).toStrictEqual(mapRoute(route, parsePageRef))
    })

    // Query
    expect(declTypes).toContain('Query')
    Object.values(Fixtures.QUERIES).forEach(({ NAME }) => {
      expect(declNames).toContain(NAME)

      const queryDecl = getDecl(result, 'Query', NAME)
      if (!queryDecl) {
        throw new Error(`Query declaration not found for ${NAME}`)
      }

      const query = userSpec.queries.get(NAME)
      if (!query) {
        throw new Error(`Query config not found for ${NAME}`)
      }
      expect(queryDecl.declValue).toStrictEqual(
        mapOperationConfig(query, parseEntityRef)
      )
    })

    // Action
    expect(declTypes).toContain('Action')
    Object.values(Fixtures.ACTIONS).forEach(({ NAME }) => {
      expect(declNames).toContain(NAME)

      const actionDecl = getDecl(result, 'Action', NAME)
      if (!actionDecl) {
        throw new Error(`Action declaration not found for ${NAME}`)
      }

      const action = userSpec.actions.get(NAME)
      if (!action) {
        throw new Error(`Action config not found for ${NAME}`)
      }
      expect(actionDecl.declValue).toStrictEqual(
        mapOperationConfig(action, parseEntityRef)
      )
    })

    // Crud
    expect(declTypes).toContain('Crud')
    Object.values(Fixtures.CRUDS).forEach(({ NAME }) => {
      expect(declNames).toContain(NAME)

      const crudDecl = getDecl(result, 'Crud', NAME)
      if (!crudDecl) {
        throw new Error(`Crud declaration not found for ${NAME}`)
      }

      const crud = userSpec.cruds.get(NAME)
      if (!crud) {
        throw new Error(`Crud config not found for ${NAME}`)
      }
      expect(crudDecl.declValue).toStrictEqual(mapCrud(crud, parseEntityRef))
    })

    // ApiNamespace
    expect(declTypes).toContain('ApiNamespace')
    Object.values(Fixtures.API_NAMESPACES).forEach(({ NAME }) => {
      expect(declNames).toContain(NAME)

      const apiNamespaceDecl = getDecl(result, 'ApiNamespace', NAME)
      if (!apiNamespaceDecl) {
        throw new Error(`ApiNamespace declaration not found for ${NAME}`)
      }

      const apiNamespace = userSpec.apiNamespaces.get(NAME)
      if (!apiNamespace) {
        throw new Error(`ApiNamespace config not found for ${NAME}`)
      }
      expect(apiNamespaceDecl.declValue).toStrictEqual(
        mapApiNamespace(apiNamespace)
      )
    })

    // Api
    expect(declTypes).toContain('Api')
    Object.values(Fixtures.APIS).forEach(({ NAME }) => {
      expect(declNames).toContain(NAME)

      const apiDecl = getDecl(result, 'Api', NAME)
      if (!apiDecl) {
        throw new Error(`Api declaration not found for ${NAME}`)
      }

      const api = userSpec.apis.get(NAME)
      if (!api) {
        throw new Error(`Api config not found for ${NAME}`)
      }
      expect(apiDecl.declValue).toStrictEqual(mapApiConfig(api, parseEntityRef))
    })

    // Job
    expect(declTypes).toContain('Job')
    Object.values(Fixtures.JOBS).forEach(({ NAME }) => {
      expect(declNames).toContain(NAME)

      const jobDecl = getDecl(result, 'Job', NAME)
      if (!jobDecl) {
        throw new Error(`Job declaration not found for ${NAME}`)
      }

      const job = userSpec.jobs.get(NAME)
      if (!job) {
        throw new Error(`Job config not found for ${NAME}`)
      }
      expect(jobDecl.declValue).toStrictEqual(mapJob(job, parseEntityRef))
    })
  })

  describe('mapApp', () => {
    test('should map minimal config correctly', () => {
      const app = new UserSpec.App(Fixtures.APP.NAME, {
        title: Fixtures.APP.CONFIG.title,
        wasp: Fixtures.APP.CONFIG.wasp,
      })
      const userSpec = app[GET_USER_SPEC]()
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
          version: Fixtures.APP.CONFIG.wasp.version,
        },
        title: Fixtures.APP.CONFIG.title,
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
      const app = new UserSpec.App(Fixtures.APP.NAME, Fixtures.APP.CONFIG)
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
          version: Fixtures.APP.CONFIG.wasp.version,
        },
        title: Fixtures.APP.CONFIG.title,
        head: Fixtures.APP.CONFIG.head,
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

  describe('mapAuth', () => {
    test('should map minimal config correctly', () => {
      const minimalAuth: UserSpec.AuthConfig = {
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
      const auth: UserSpec.AuthConfig = {
        userEntity: Fixtures.AUTH.CONFIG.userEntity,
        methods: {},
        onAuthFailedRedirectTo: Fixtures.AUTH.CONFIG.onAuthFailedRedirectTo,
      }
      const parseEntityRef = makeRefParser('Entity', [])
      const parseRouteRef = makeRefParser('Route', [])

      expect(() => mapAuth(auth, parseEntityRef, parseRouteRef)).toThrowError()
    })

    test('should throw if externalAuthEntity ref is not provided when defined', () => {
      const auth: UserSpec.AuthConfig = {
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
      const minimalEmailAuth: UserSpec.EmailAuthConfig = {
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
      const emailAuth: UserSpec.EmailAuthConfig = {
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
      const emailAuth: UserSpec.EmailAuthConfig = {
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
      const minimalEmailVerification: UserSpec.EmailVerificationConfig = {
        clientRoute:
          Fixtures.AUTH.CONFIG.methods.email.emailVerification.clientRoute,
      }
      const parseRouteRef = makeRefParser('Route', [
        minimalEmailVerification.clientRoute,
      ])

      const result = mapEmailVerification(
        minimalEmailVerification,
        parseRouteRef
      )

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
      const minimalPasswordReset: UserSpec.PasswordResetConfig = {
        clientRoute:
          Fixtures.AUTH.CONFIG.methods.email.passwordReset.clientRoute,
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
      const passwordResetConfig =
        Fixtures.AUTH.CONFIG.methods.email.passwordReset
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
      const minimalUsernameAndPassword: UserSpec.UsernameAndPasswordConfig = {}

      const result = mapUsernameAndPassword(minimalUsernameAndPassword)

      expect(result).toStrictEqual({
        userSignupFields: undefined,
      } satisfies AppSpec.UsernameAndPasswordConfig)
    })

    test('should map full config correctly', () => {
      const usernameAndPassword =
        Fixtures.AUTH.CONFIG.methods.usernameAndPassword

      const result = mapUsernameAndPassword(usernameAndPassword)

      expect(result).toStrictEqual({
        userSignupFields: mapExtImport(usernameAndPassword.userSignupFields),
      } satisfies AppSpec.UsernameAndPasswordConfig)
    })
  })

  describe('mapExternalAuth', () => {
    test('should map minimal config correctly', () => {
      const minimalExternalAuth: UserSpec.ExternalAuthConfig = {}

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
      const minimalClient: UserSpec.ClientConfig = {}

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
      const minimalServer: UserSpec.ServerConfig = {}

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
      const minimalEmailSender: UserSpec.EmailSender = {
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
      const minimalWebsocket: UserSpec.WebsocketConfig = {
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
      const minimalDb: UserSpec.DbConfig = {}

      const result = mapDb(minimalDb)

      expect(result).toStrictEqual({
        seeds: undefined,
      } satisfies AppSpec.Db)
    })

    test('should map full config correctly', () => {
      const db: UserSpec.DbConfig = {
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
      const query: UserSpec.QueryConfig = Fixtures.QUERIES.FULL.CONFIG
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
      const crud: UserSpec.Crud = Fixtures.CRUDS.FULL.CONFIG
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
      const crudOperationOptions = Fixtures.CRUDS.FULL.CONFIG.operations.get

      const result = mapCrudOperationOptions(crudOperationOptions)

      expect(result).toStrictEqual({
        isPublic: crudOperationOptions.isPublic,
        overrideFn: mapExtImport(crudOperationOptions.overrideFn),
      } satisfies AppSpec.CrudOperationOptions)
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

    test('should map default import correctly', () => {
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

    // test('should throw for having both import kind', () => {
    //   const extImport: UserSpec.ExtImport = {
    //     import: 'myNamedImport',
    //     from: '@src/myModule',
    //     importDefault: 'myDefaultImport',
    //   }

    //   expect(() => mapExtImport(extImport)).toThrowError()
    // })

    test('should throw for missing import kind', () => {
      const extImport: UserSpec.ExtImport = {
        from: '@src/myModule',
      } as unknown as UserSpec.ExtImport

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
})
