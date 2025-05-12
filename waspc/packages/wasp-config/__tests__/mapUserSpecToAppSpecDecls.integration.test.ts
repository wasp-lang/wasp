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
      configMap: userSpec.pages,
      expectedMapping: {
        function: mapPage,
      },
    })
    expectCorrectMapping({
      declType: 'Route',
      configMap: userSpec.routes,
      expectedMapping: {
        function: mapRoute,
        extraArgs: [pageRefParser],
      },
    })
    expectCorrectMapping({
      declType: 'Query',
      configMap: userSpec.queries,
      expectedMapping: {
        function: mapOperation,
        extraArgs: [entityRefParser],
      },
    })
    expectCorrectMapping({
      declType: 'Action',
      configMap: userSpec.actions,
      expectedMapping: {
        function: mapOperation,
        extraArgs: [entityRefParser],
      },
    })
    expectCorrectMapping({
      declType: 'Crud',
      configMap: userSpec.cruds,
      expectedMapping: {
        function: mapCrud,
        extraArgs: [entityRefParser],
      },
    })
    expectCorrectMapping({
      declType: 'ApiNamespace',
      configMap: userSpec.apiNamespaces,
      expectedMapping: {
        function: mapApiNamespace,
      },
    })
    expectCorrectMapping({
      declType: 'Api',
      configMap: userSpec.apis,
      expectedMapping: {
        function: mapApi,
        extraArgs: [entityRefParser],
      },
    })
    expectCorrectMapping({
      declType: 'Job',
      configMap: userSpec.jobs,
      expectedMapping: {
        function: mapJob,
        extraArgs: [entityRefParser],
      },
    })

    function expectCorrectMapping({
      declType,
      configMap,
      expectedMapping,
    }: {
      declType: keyof AppSpec.DeclTypeToValue
      configMap: Map<string, any>
      expectedMapping: {
        function: (item: any, ...args: any[]) => any
        extraArgs?: any[]
      }
    }): void {
      const { function: mappingFn, extraArgs = [] } = expectedMapping
      configMap.forEach((config, name) => {
        const decl = getDecl(result, declType, name)

        expect(decl).toStrictEqual({
          declType,
          declName: name,
          declValue: mappingFn(config, ...extraArgs),
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
