/* eslint-disable @typescript-eslint/no-explicit-any */
import { afterEach } from 'node:test'
import { describe, expect, test, vi } from 'vitest'
import { analyzeUserApp, getUserApp, Result } from '../src/appAnalyzer'
import * as UserApi from '../src/userApi'
import { createUserApp } from './testFixtures'

describe('analyzeUserApp', () => {
  afterEach(() => vi.clearAllMocks())

  async function testAnalyzeUserApp(
    mockUserApp: any,
    mockEntities: string[],
    validateResult: (result: Result<string, string>) => void
  ): Promise<void> {
    const mockMainWaspTs = 'main.wasp.ts'
    vi.doMock(mockMainWaspTs, () => ({ default: mockUserApp }))

    const result = await analyzeUserApp(mockMainWaspTs, mockEntities)

    validateResult(result)
  }

  test('should parse minimal app sucessfully', async () => {
    const mockUserApp = createUserApp('minimal')
    await testAnalyzeUserApp(mockUserApp, [], (result) => {
      if (result.status !== 'ok') {
        throw new Error('Expected a successful result, but got an error.')
      }
      expect(result).toStrictEqual({
        status: 'ok',
        value:
          '[{"declType":"App","declName":"minimalApp","declValue":{"wasp":{"version":"0.16.3"},"title":"Minimal App"}}]',
      })
    })
  })

  test('should fail if wasp config is faulty', async () => {
    const mockUserApp = undefined
    await testAnalyzeUserApp(mockUserApp, [], (result) => {
      if (result.status !== 'error') {
        throw new Error('Expected an error, but got a successful result.')
      }
      expect(result.error).toBeDefined()
    })
  })
})

describe('getUserApp', () => {
  afterEach(() => vi.clearAllMocks())

  async function testGetUserApp(
    moockUserApp: any,
    validateResult: (result: Result<UserApi.App, string>) => void
  ): Promise<void> {
    const mockFileName = 'main.wasp.ts'
    vi.doMock(mockFileName, () => moockUserApp)

    const result = await getUserApp(mockFileName)

    validateResult(result)
  }

  test('should return an error if the default export is undefined', async () => {
    const mockUserApp = undefined
    await testGetUserApp({ default: mockUserApp }, (result) => {
      if (result.status !== 'error') {
        throw new Error('Expected an error, but got a successful result.')
      }
      expect(result.error).toContain('default export')
    })
  })

  test('should return an error if the default export is not an instance of App', async () => {
    const mockUserApp = 'not an instance of App'
    await testGetUserApp({ default: mockUserApp }, (result) => {
      if (result.status !== 'error') {
        throw new Error('Expected an error, but got a successful result.')
      }
      expect(result.error).toContain('must be an instance of App')
    })
  })

  test('should return the user app if the default export is valid', async () => {
    const mockUserApp = createUserApp('minimal')
    await testGetUserApp({ default: mockUserApp }, (result) => {
      if (result.status !== 'ok') {
        throw new Error('Expected a successful result, but got an error.')
      }
      expect(result.value).toBe(mockUserApp)
    })
  })
})
