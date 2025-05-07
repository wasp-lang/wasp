import { afterEach } from 'node:test'
import { describe, expect, test, vi } from 'vitest'
import { analyzeUserApp, getUserApp } from '../src/appAnalyzer'
import { createFullUserApp, createMinimalUserApp } from './testFixtures'

describe('analyzeUserApp', () => {
  afterEach(() => {
    vi.clearAllMocks()
  })

  test('should parse minimal app sucessfully', async () => {
    const mockMainWaspTs = 'mockMain.wasp.ts'
    const mockUserApp = createMinimalUserApp()

    vi.doMock(mockMainWaspTs, () => {
      return {
        default: mockUserApp,
      }
    })

    const result = await analyzeUserApp(mockMainWaspTs, [])

    if (result.status !== 'ok') {
      throw new Error('Expected a successful result, but got an error.')
    }

    expect(result.value).toContain(
      '[{"declType":"App","declName":"minimalApp","declValue":{"wasp":{"version":"0.16.3"},"title":"Minimal App"}}]'
    )
  })

  test('should fail if wasp config is faulty', async () => {
    const mockMainWaspTs = 'mockMain.wasp.ts'

    vi.doMock(mockMainWaspTs, () => {
      return {
        default: undefined,
      }
    })

    const result = await analyzeUserApp(mockMainWaspTs, [])

    if (result.status !== 'error') {
      throw new Error('Expected an error, but got a successful result.')
    }
    expect(result.error).toBeDefined()
  })
})

describe('getUserApp', () => {
  afterEach(() => {
    vi.clearAllMocks()
  })

  test('should return an error if the default export is undefined', async () => {
    const mockMainWaspTs = 'mockMain.wasp.ts'

    vi.doMock(mockMainWaspTs, () => {
      return {
        default: undefined,
      }
    })

    const result = await getUserApp(mockMainWaspTs)

    if (result.status !== 'error') {
      throw new Error('Expected an error, but got a successful result.')
    }
    expect(result.error).toContain('default export')
  })

  test('should return an error if the default export is not an instance of App', async () => {
    const mockMainWaspTs = 'mockMain.wasp.ts'

    vi.doMock(mockMainWaspTs, () => {
      return {
        default: 'not an instance of App',
      }
    })

    const result = await getUserApp(mockMainWaspTs)

    if (result.status !== 'error') {
      throw new Error('Expected an error, but got a successful result.')
    }
    expect(result.error).toContain('must be an instance of App')
  })

  test('should return the user app if the default export is valid', async () => {
    const mockMainWaspTs = 'mockMain.wasp.ts'
    const mockUserApp = createFullUserApp()

    vi.doMock(mockMainWaspTs, () => {
      return {
        default: mockUserApp,
      }
    })

    const result = await getUserApp(mockMainWaspTs)

    if (result.status !== 'ok') {
      throw new Error('Expected a successful result, but got an error.')
    }
    expect(result.value).toBe(mockUserApp)
  })
})
