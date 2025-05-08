/* eslint-disable @typescript-eslint/no-explicit-any */
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
  test('should map full app using mapping functions correctly', () => {
    const userSpec = Fixtures.createUserSpec('full')
    const entityRefParser = makeRefParser('Entity', Fixtures.ALL_ENTITIES)
    const routeRefParser = makeRefParser('Route', Fixtures.ALL_ROUTE_NAMES)
    const pageRefParser = makeRefParser('Page', Fixtures.ALL_PAGE_NAMES)

    const result = mapUserSpecToAppSpecDecls(userSpec, Fixtures.ALL_ENTITIES)

    const appDecl = getDecl(result, 'App', Fixtures.APP.FULL.NAME)
    expect(appDecl.declValue).toStrictEqual(
      mapApp(
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
    )
    expectCorrectMapping({
      declType: 'Page',
      declFixtures: Fixtures.PAGES,
      declMap: userSpec.pages,
      mapUserSpecToAppSpec: mapPage,
    })
    expectCorrectMapping({
      declType: 'Route',
      declFixtures: Fixtures.ROUTES,
      declMap: userSpec.routes,
      mapUserSpecToAppSpec: mapRoute,
      parserArgs: [pageRefParser],
    })
    expectCorrectMapping({
      declType: 'Query',
      declFixtures: Fixtures.QUERIES,
      declMap: userSpec.queries,
      mapUserSpecToAppSpec: mapOperationConfig,
      parserArgs: [entityRefParser],
    })
    expectCorrectMapping({
      declType: 'Action',
      declFixtures: Fixtures.ACTIONS,
      declMap: userSpec.actions,
      mapUserSpecToAppSpec: mapOperationConfig,
      parserArgs: [entityRefParser],
    })
    expectCorrectMapping({
      declType: 'Crud',
      declFixtures: Fixtures.CRUDS,
      declMap: userSpec.cruds,
      mapUserSpecToAppSpec: mapCrud,
      parserArgs: [entityRefParser],
    })
    expectCorrectMapping({
      declType: 'ApiNamespace',
      declFixtures: Fixtures.API_NAMESPACES,
      declMap: userSpec.apiNamespaces,
      mapUserSpecToAppSpec: mapApiNamespace,
    })
    expectCorrectMapping({
      declType: 'Api',
      declFixtures: Fixtures.APIS,
      declMap: userSpec.apis,
      mapUserSpecToAppSpec: mapApiConfig,
      parserArgs: [entityRefParser],
    })
    expectCorrectMapping({
      declType: 'Job',
      declFixtures: Fixtures.JOBS,
      declMap: userSpec.jobs,
      mapUserSpecToAppSpec: mapJob,
      parserArgs: [entityRefParser],
    })

    type DeclFixtures = Record<string, { NAME: string; CONFIG: any }>
    function expectCorrectMapping({
      declType,
      declFixtures,
      declMap,
      mapUserSpecToAppSpec,
      parserArgs = [],
    }: {
      declType: keyof AppSpec.DeclTypeToValue
      declFixtures: DeclFixtures
      declMap: Map<string, any>
      mapUserSpecToAppSpec: (item: any, ...args: any[]) => any
      parserArgs?: any[]
    }): void {
      Object.values(declFixtures).forEach(({ NAME }) => {
        const decl = getDecl(result, declType, NAME)

        const item = declMap.get(NAME)
        if (!item) {
          throw new Error(`${declType} config not found for ${NAME}`)
        }

        expect(decl.declValue).toStrictEqual(
          mapUserSpecToAppSpec(item, ...parserArgs)
        )
      })
    }
  })

  test('should map mininmal app using mapping functions correctly', () => {
    const userSpec = Fixtures.createUserSpec('minimal')
    const entityRefParser = makeRefParser('Entity', [])
    const routeRefParser = makeRefParser('Route', [])

    const result = mapUserSpecToAppSpecDecls(userSpec, [])

    const appDecl = getDecl(result, 'App', Fixtures.APP.MINIMAL.NAME)
    expect(appDecl.declValue).toStrictEqual(
      mapApp(
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
    )
  })

  /**
   * Retrieves a specific declaration from a list of declarations based on its type and name.
   * @returns The matching declaration if found.
   * @throws An error if the declaration is not found.
   */
  function getDecl<T extends keyof AppSpec.DeclTypeToValue>(
    decls: AppSpec.Decl[],
    declType: T,
    declName: string
  ): AppSpec.GetDeclForType<T> {
    const decl = decls.find(
      (decl): decl is AppSpec.GetDeclForType<T> =>
        decl.declType === declType && decl.declName === declName
    )

    if (!decl) {
      throw new Error(`${declType} declaration "${declName}" not found`)
    }

    return decl
  }
})
