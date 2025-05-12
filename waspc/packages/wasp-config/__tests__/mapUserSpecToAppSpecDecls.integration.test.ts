/* eslint-disable @typescript-eslint/no-explicit-any */
import { describe, expect, test } from 'vitest'
import { GET_USER_SPEC } from '../src/_private.js'
import * as AppSpec from '../src/appSpec.js'
import {
  makeRefParser,
  mapApi,
  mapApiNamespace,
  mapApp,
  mapCrud,
  mapJob,
  mapOperation,
  mapPage,
  mapRoute,
  mapUserSpecToAppSpecDecls,
} from '../src/mapUserSpecToAppSpecDecls.js'
import * as Fixtures from './testFixtures.js'

describe('mapUserSpecToAppSpecDecls', () => {
  test('should map full app using mapping functions correctly', () => {
    const { appName, userApp } = Fixtures.createUserApp('full')
    const userSpec = userApp[GET_USER_SPEC]()
    const entities = Fixtures.getEntities('full')
    const entityRefParser = makeRefParser('Entity', entities)
    const routeRefParser = makeRefParser(
      'Route',
      Fixtures.getRoutes().map((r) => r.name)
    )
    const pageRefParser = makeRefParser(
      'Page',
      Fixtures.getPages().map((p) => p.name)
    )
    const appDeclType = 'App'

    const result = mapUserSpecToAppSpecDecls(userSpec, entities)

    const appDecl = getDecl(result, appDeclType, appName)
    expect(appDecl).toStrictEqual({
      declType: appDeclType,
      declName: appName,
      declValue: mapApp(
        userSpec.app.config,
        entityRefParser,
        routeRefParser,
        userSpec.auth,
        userSpec.server,
        userSpec.client,
        userSpec.db,
        userSpec.emailSender,
        userSpec.websocket
      ),
    })
    expectCorrectMapping({
      declType: 'Page',
      declFixtures: Fixtures.getPages(),
      declMap: userSpec.pages,
      mapUserSpecToAppSpec: mapPage,
    })
    expectCorrectMapping({
      declType: 'Route',
      declFixtures: Fixtures.getRoutes(),
      declMap: userSpec.routes,
      mapUserSpecToAppSpec: mapRoute,
      parserArgs: [pageRefParser],
    })
    expectCorrectMapping({
      declType: 'Query',
      declFixtures: Fixtures.getQueries(),
      declMap: userSpec.queries,
      mapUserSpecToAppSpec: mapOperation,
      parserArgs: [entityRefParser],
    })
    expectCorrectMapping({
      declType: 'Action',
      declFixtures: Fixtures.getActions(),
      declMap: userSpec.actions,
      mapUserSpecToAppSpec: mapOperation,
      parserArgs: [entityRefParser],
    })
    expectCorrectMapping({
      declType: 'Crud',
      declFixtures: Fixtures.getCruds(),
      declMap: userSpec.cruds,
      mapUserSpecToAppSpec: mapCrud,
      parserArgs: [entityRefParser],
    })
    expectCorrectMapping({
      declType: 'ApiNamespace',
      declFixtures: Fixtures.getApiNamespaces(),
      declMap: userSpec.apiNamespaces,
      mapUserSpecToAppSpec: mapApiNamespace,
    })
    expectCorrectMapping({
      declType: 'Api',
      declFixtures: Fixtures.getApis(),
      declMap: userSpec.apis,
      mapUserSpecToAppSpec: mapApi,
      parserArgs: [entityRefParser],
    })
    expectCorrectMapping({
      declType: 'Job',
      declFixtures: Fixtures.getJobs(),
      declMap: userSpec.jobs,
      mapUserSpecToAppSpec: mapJob,
      parserArgs: [entityRefParser],
    })

    function expectCorrectMapping({
      declType,
      declFixtures,
      declMap,
      mapUserSpecToAppSpec,
      parserArgs = [],
    }: {
      declType: keyof AppSpec.DeclTypeToValue
      declFixtures: Fixtures.NamedConfig<unknown>[]
      declMap: Map<string, any>
      mapUserSpecToAppSpec: (item: any, ...args: any[]) => any
      parserArgs?: any[]
    }): void {
      declFixtures.forEach(({ name }) => {
        const decl = getDecl(result, declType, name)

        const item = declMap.get(name)
        expect(item).toBeDefined()

        expect(decl).toStrictEqual({
          declType,
          declName: name,
          declValue: mapUserSpecToAppSpec(item, ...parserArgs),
        })
      })
    }
  })

  test('should map minimal app using mapping functions correctly', () => {
    const { appName, userApp } = Fixtures.createUserApp('minimal')
    const userSpec = userApp[GET_USER_SPEC]()
    const entities = Fixtures.getEntities('minimal')
    const entityRefParser = makeRefParser('Entity', entities)
    const routeRefParser = makeRefParser('Route', [])
    const appDeclType = 'App'

    const result = mapUserSpecToAppSpecDecls(userSpec, entities)

    const appDecl = getDecl(result, appDeclType, appName)
    expect(appDecl).toStrictEqual({
      declType: appDeclType,
      declName: appName,
      declValue: mapApp(
        userSpec.app.config,
        entityRefParser,
        routeRefParser,
        userSpec.auth,
        userSpec.server,
        userSpec.client,
        userSpec.db,
        userSpec.emailSender,
        userSpec.websocket
      ),
    })
  })

  /**
   * Retrieves a specific declaration from a list of declarations based on its type and name.
   */
  function getDecl<T extends keyof AppSpec.DeclTypeToValue>(
    decls: AppSpec.Decl[],
    declType: T,
    declName: string
  ): AppSpec.GetDeclForType<T> | undefined {
    const decl = decls.find(
      (decl): decl is AppSpec.GetDeclForType<T> =>
        decl.declType === declType && decl.declName === declName
    )

    return decl
  }
})
