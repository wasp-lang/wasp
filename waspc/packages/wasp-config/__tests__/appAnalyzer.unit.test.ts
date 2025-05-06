import { afterEach } from 'node:test'
import { describe, expect, test, vi } from 'vitest'
import { getUserApp } from '../src/appAnalyzer'
import { createFullUserApp } from './testFixtures'

describe('getUserApp', () => {
  afterEach(() => {
    vi.clearAllMocks()
  })

  test('should return an error if the default export is undefined', async () => {
    const mockMainWaspJs = 'mockMain.wasp.js'

    vi.doMock('mockMain.wasp.js', () => {
      return {
        default: undefined,
      }
    })

    const result = await getUserApp(mockMainWaspJs)

    if (result.status !== 'error') {
      throw new Error('Expected an error, but got a successful result.')
    }
    expect(result.error).toContain('default export')
  })

  test('should return an error if the default export is not an instance of App', async () => {
    const mockMainWaspJs = 'mockMain.wasp.js'

    vi.doMock('mockMain.wasp.js', () => {
      return {
        default: 'not an instance of App',
      }
    })

    const result = await getUserApp(mockMainWaspJs)

    if (result.status !== 'error') {
      throw new Error('Expected an error, but got a successful result.')
    }
    expect(result.error).toContain('must be an instance of App')
  })

  test('should return the user app if the default export is valid', async () => {
    const mockMainWaspJs = 'mockMain.wasp.js'
    const mockUserApp = createFullUserApp()

    vi.doMock('mockMain.wasp.js', () => {
      return {
        default: mockUserApp,
      }
    })

    const result = await getUserApp(mockMainWaspJs)

    if (result.status !== 'ok') {
      throw new Error('Expected a successful result, but got an error.')
    }
    expect(result.value).toBe(mockUserApp)
  })
})
