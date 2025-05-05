import { describe, expect, test } from 'vitest'
import { GET_USER_SPEC } from '../src/_private.js'
import * as AppSpec from '../src/appSpec.js'
import {
  makeRefParser,
  mapApiConfig,
  mapApiNamespace,
  mapApp,
  mapCrud,
  mapJob,
  mapOperationConfig,
  mapPage,
  mapRoute,
  mapUserSpecToAppSpecDecls,
} from '../src/mapUserSpecToAppSpecDecls.js'
import * as UserSpec from '../src/userApi.js'
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
    declName: string
  ): AppSpec.GetDeclForType<T> | undefined {
    return decls.find(
      (decl): decl is AppSpec.GetDeclForType<T> =>
        decl.declType === declType && decl.declName === declName
    )
  }

  // This test deliberately avoids using individual mapping functions and instead uses raw values.
  // This serves as an integration test to ensure the complete UserSpec to AppSpec transformation
  // pipeline works correctly, independent of the individual mapping functions tested below.
  test('should map without mapping functions correctly', () => {
    const userSpec = createFullUserSpec()
    const result = mapUserSpecToAppSpecDecls(userSpec, Fixtures.ALL_ENTITIES)

    const declTypes = result.map((decl) => decl.declType)
    const declNames = result.map((decl) => decl.declName)

    // AppConfig
    expect(declTypes).toContain('App')
    expect(declNames).toContain(Fixtures.APP.NAME)

    const appDecl = getDecl(result, 'App', Fixtures.APP.NAME)
    if (!appDecl) {
      throw new Error('App declaration not found')
    }

    expect(appDecl).toBeDefined()
    expect(appDecl.declValue.title).toBe(Fixtures.APP.CONFIG.title)
    expect(appDecl.declValue.wasp.version).toBe(
      Fixtures.APP.CONFIG.wasp.version
    )
    expect(appDecl.declValue.head).toHaveLength(
      Fixtures.APP.CONFIG.head?.length
    )
    Fixtures.APP.CONFIG.head?.forEach((head, index) => {
      expect(appDecl.declValue.head?.[index]).toBe(head)
    })

    // Auth
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

    // Client
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

    // Server
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

    // EmailSender
    const emailSender = appDecl.declValue.emailSender

    expect(emailSender).toBeDefined()
    expect(emailSender?.provider).toBe(Fixtures.EMAIL_SENDER.CONFIG.provider)
    expect(emailSender?.defaultFrom?.email).toBe(
      Fixtures.EMAIL_SENDER.CONFIG.defaultFrom.email
    )
    expect(emailSender?.defaultFrom?.name).toBe(
      Fixtures.EMAIL_SENDER.CONFIG.defaultFrom.name
    )

    // WebSocket
    const webSocket = appDecl.declValue.webSocket

    expect(webSocket).toBeDefined()
    expect(webSocket?.fn).toStrictEqual({
      kind: 'named',
      name: Fixtures.WEBSOCKET.CONFIG.fn.import,
      path: Fixtures.WEBSOCKET.CONFIG.fn.from,
    })

    // Db
    const db = appDecl.declValue.db

    expect(db).toBeDefined()
    expect(db?.seeds).toHaveLength(1)
    expect(db?.seeds?.[0]).toStrictEqual({
      kind: 'named',
      name: Fixtures.DB.CONFIG.seeds[0]?.import,
      path: Fixtures.DB.CONFIG.seeds[0]?.from,
    })

    // Page
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

    // Route
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

    // Query
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

    // Action
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

    // Crud
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

    // ApiNamespace
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

    // Api
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

    // Job
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

  test('should map using mapping functions correctly', () => {
    const userSpec = createFullUserSpec()
    const parseEntityRef = makeRefParser('Entity', Fixtures.ALL_ENTITIES)
    const parseRouteRef = makeRefParser('Route', Fixtures.ALL_ROUTE_NAMES)
    const parsePageRef = makeRefParser('Page', Fixtures.ALL_PAGE_NAMES)

    const result = mapUserSpecToAppSpecDecls(userSpec, Fixtures.ALL_ENTITIES)

    const declTypes = result.map((decl) => decl.declType)
    const declNames = result.map((decl) => decl.declName)

    // App
    expect(declTypes).toContain('App')
    expect(declNames).toContain(Fixtures.APP.NAME)

    const appDecl = getDecl(result, 'App', Fixtures.APP.NAME)
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
})
