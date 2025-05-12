import { afterEach } from 'node:test'
import { describe, expect, test, vi } from 'vitest'
import { GET_USER_SPEC } from '../src/_private'
import { analyzeUserApp } from '../src/appAnalyzer'
import { mapUserSpecToAppSpecDecls } from '../src/mapUserSpecToAppSpecDecls'
import * as UserApi from '../src/userApi'
import * as Fixtures from './testFixtures.js'

describe('analyzeUserApp', () => {
  afterEach(() => vi.clearAllMocks())

  test('should parse minimal app sucessfully', async () => {
    await testAnalyzeUserApp({
      app: Fixtures.createUserApp('minimal'),
      entities: Fixtures.getEntities('minimal'),
    })
  })

  test('should parse full app sucessfully', async () => {
    await testAnalyzeUserApp({
      app: Fixtures.createUserApp('full'),
      entities: Fixtures.getEntities('full'),
    })
  })

  test('should return an error if the default export is not defined', async () => {
    await testAnalyzeUserApp(
      {
        app: undefined as unknown as UserApi.App,
        entities: Fixtures.getEntities('minimal'),
      },
      {
        shouldReturnError: true,
      }
    )
  })

  test('should return an error if the default export is not an instance of App', async () => {
    await testAnalyzeUserApp(
      {
        app: 'not an instance of App' as unknown as UserApi.App,
        entities: Fixtures.getEntities('minimal'),
      },
      {
        shouldReturnError: true,
      }
    )
  })

  async function testAnalyzeUserApp(
    input: {
      app: UserApi.App
      entities: string[]
    },
    options:
      | {
          shouldReturnError: boolean
        }
      | undefined = {
      shouldReturnError: false,
    }
  ): Promise<void> {
    const { app, entities } = input
    const { shouldReturnError } = options
    const mockMainWaspTs = 'main.wasp.ts'
    vi.doMock(mockMainWaspTs, () => ({ default: app }))

    const result = await analyzeUserApp(mockMainWaspTs, entities)

    if (shouldReturnError) {
      expect(result).toMatchObject({
        status: 'error',
        error: expect.anything(),
      })
    } else {
      const userSpec = app[GET_USER_SPEC]()
      const appSpecDecls = mapUserSpecToAppSpecDecls(userSpec, entities)

      expect(result).toMatchObject({
        status: 'ok',
        value: appSpecDecls,
      })
    }
  }
})
