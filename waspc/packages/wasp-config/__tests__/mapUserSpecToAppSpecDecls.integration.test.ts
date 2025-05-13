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
    expectCorrectDeclMapping({
      declType: 'Page',
      decls: userSpec.pages,
      expectedMapping: {
        function: mapPage,
      },
      actualMapping: result,
    })
    expectCorrectDeclMapping({
      declType: 'Route',
      decls: userSpec.routes,
      expectedMapping: {
        function: mapRoute,
        extraArgs: [pageRefParser],
      },
      actualMapping: result,
    })
    expectCorrectDeclMapping({
      declType: 'Query',
      decls: userSpec.queries,
      expectedMapping: {
        function: mapOperation,
        extraArgs: [entityRefParser],
      },
      actualMapping: result,
    })
    expectCorrectDeclMapping({
      declType: 'Action',
      decls: userSpec.actions,
      expectedMapping: {
        function: mapOperation,
        extraArgs: [entityRefParser],
      },
      actualMapping: result,
    })
    expectCorrectDeclMapping({
      declType: 'Crud',
      decls: userSpec.cruds,
      expectedMapping: {
        function: mapCrud,
        extraArgs: [entityRefParser],
      },
      actualMapping: result,
    })
    expectCorrectDeclMapping({
      declType: 'ApiNamespace',
      decls: userSpec.apiNamespaces,
      expectedMapping: {
        function: mapApiNamespace,
      },
      actualMapping: result,
    })
    expectCorrectDeclMapping({
      declType: 'Api',
      decls: userSpec.apis,
      expectedMapping: {
        function: mapApi,
        extraArgs: [entityRefParser],
      },
      actualMapping: result,
    })
    expectCorrectDeclMapping({
      declType: 'Job',
      decls: userSpec.jobs,
      expectedMapping: {
        function: mapJob,
        extraArgs: [entityRefParser],
      },
      actualMapping: result,
    })
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

  function expectCorrectDeclMapping({
    declType,
    decls,
    expectedMapping,
    actualMapping,
  }: {
    declType: keyof AppSpec.DeclTypeToValue
    decls: Map<string, any>
    expectedMapping: {
      function: (item: any, ...args: any[]) => any
      extraArgs?: any[]
    }
    actualMapping: AppSpec.Decl[]
  }): void {
    const { function: mappingFn, extraArgs = [] } = expectedMapping
    decls.forEach((config, name) => {
      const resultDecl = getDecl(actualMapping, declType, name)

      expect(resultDecl).toStrictEqual({
        declType,
        declName: name,
        declValue: mappingFn(config, ...extraArgs),
      })
    })
  }

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
