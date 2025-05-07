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

test('should map using mapping functions correctly', () => {
  describe('mapUserSpecToAppSpecDecls', () => {
    const userSpec = Fixtures.createFullUserSpec()
    const parseEntityRef = makeRefParser('Entity', Fixtures.ALL_ENTITIES)
    const parseRouteRef = makeRefParser('Route', Fixtures.ALL_ROUTE_NAMES)
    const parsePageRef = makeRefParser('Page', Fixtures.ALL_PAGE_NAMES)

    const result = mapUserSpecToAppSpecDecls(userSpec, Fixtures.ALL_ENTITIES)

    const appDecl = getDecl(result, 'App', Fixtures.APP.FULL.NAME)
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
    testDeclMapping({
      declType: 'Page',
      fixtures: Fixtures.PAGES,
      property: 'pages',
      mapper: mapPage,
    })
    testDeclMapping({
      declType: 'Route',
      fixtures: Fixtures.ROUTES,
      property: 'routes',
      mapper: mapRoute,
      extraArgs: [parsePageRef],
    })
    testDeclMapping({
      declType: 'Query',
      fixtures: Fixtures.QUERIES,
      property: 'queries',
      mapper: mapOperationConfig,
      extraArgs: [parseEntityRef],
    })
    testDeclMapping({
      declType: 'Action',
      fixtures: Fixtures.ACTIONS,
      property: 'actions',
      mapper: mapOperationConfig,
      extraArgs: [parseEntityRef],
    })
    testDeclMapping({
      declType: 'Crud',
      fixtures: Fixtures.CRUDS,
      property: 'cruds',
      mapper: mapCrud,
      extraArgs: [parseEntityRef],
    })
    testDeclMapping({
      declType: 'ApiNamespace',
      fixtures: Fixtures.API_NAMESPACES,
      property: 'apiNamespaces',
      mapper: mapApiNamespace,
    })
    testDeclMapping({
      declType: 'Api',
      fixtures: Fixtures.APIS,
      property: 'apis',
      mapper: mapApiConfig,
      extraArgs: [parseEntityRef],
    })
    testDeclMapping({
      declType: 'Job',
      fixtures: Fixtures.JOBS,
      property: 'jobs',
      mapper: mapJob,
      extraArgs: [parseEntityRef],
    })

    function testDeclMapping<T extends keyof AppSpec.DeclTypeToValue>({
      declType,
      fixtures,
      property,
      mapper,
      extraArgs = [],
    }: {
      declType: T
      fixtures: Record<string, { NAME: string }>
      property: string
      mapper: (item: any, ...args: any[]) => any
      extraArgs?: any[]
    }): void {
      Object.values(fixtures).forEach(({ NAME }) => {
        const decl = getDecl(result, declType, NAME)

        const item = userSpec[property].get(NAME)
        if (!item) {
          throw new Error(`${declType} config not found for ${NAME}`)
        }

        expect(decl.declValue).toStrictEqual(mapper(item, ...extraArgs))
      })
    }
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
