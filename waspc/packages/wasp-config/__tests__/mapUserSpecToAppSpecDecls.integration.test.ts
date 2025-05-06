import { describe, expect, test } from 'vitest'
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
import * as Fixtures from './testFixtures.js'

describe('mapUserSpecToAppSpecDecls', () => {
  test('should map using mapping functions correctly', () => {
    const userSpec = Fixtures.createFullUserSpec()
    const parseEntityRef = makeRefParser('Entity', Fixtures.ALL_ENTITIES)
    const parseRouteRef = makeRefParser('Route', Fixtures.ALL_ROUTE_NAMES)
    const parsePageRef = makeRefParser('Page', Fixtures.ALL_PAGE_NAMES)

    const result = mapUserSpecToAppSpecDecls(userSpec, Fixtures.ALL_ENTITIES)

    // App
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
    Object.values(Fixtures.PAGES).forEach(({ NAME }) => {
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
    Object.values(Fixtures.ROUTES).forEach(({ NAME }) => {
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
    Object.values(Fixtures.QUERIES).forEach(({ NAME }) => {
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
    Object.values(Fixtures.ACTIONS).forEach(({ NAME }) => {
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
    Object.values(Fixtures.CRUDS).forEach(({ NAME }) => {
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
    Object.values(Fixtures.API_NAMESPACES).forEach(({ NAME }) => {
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
    Object.values(Fixtures.APIS).forEach(({ NAME }) => {
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
    Object.values(Fixtures.JOBS).forEach(({ NAME }) => {
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

  /**
   * Retrieves a specific declaration from a list of declarations based on its type and name.
   * @returns The matching declaration if found, or `undefined` if not found.
   */
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
})
