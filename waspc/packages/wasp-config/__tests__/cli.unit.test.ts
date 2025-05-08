import { describe, expect, test } from 'vitest'
import { parseProcessArgsOrThrow } from '../src/cli'

const noEnttiyArgs = ['node', 'run.js', 'main.wasp.js', 'output.json']

describe('parseProcessArgsOrThrow', () => {
  test('should parse arguments correctly', () => {
    const args = [...noEnttiyArgs, JSON.stringify(['entity1', 'entity2'])]

    const result = parseProcessArgsOrThrow(args)

    expect(result).toEqual({
      waspTsSpecPath: 'main.wasp.js',
      outputFilePath: 'output.json',
      entityNames: ['entity1', 'entity2'],
    })
  })

  test('should parse 0 entities correctly', () => {
    const noEntitiesArgs = [...noEnttiyArgs, '[]']

    const result = parseProcessArgsOrThrow(noEntitiesArgs)

    expect(result).toEqual({
      waspTsSpecPath: 'main.wasp.js',
      outputFilePath: 'output.json',
      entityNames: [],
    })
  })

  test('should throw an error if less than 5 arguments', () => {
    const notEnoughArgs = [...noEnttiyArgs]

    expect(() => parseProcessArgsOrThrow(notEnoughArgs)).toThrowError()
  })

  test('should throw an error if more than 5 arguments', () => {
    const tooMuchArgs = [...noEnttiyArgs, '[]', 'extraArg']

    expect(() => parseProcessArgsOrThrow(tooMuchArgs)).toThrowError()
  })

  test('should throw an error if any argument is not a string', () => {
    const pathToMainWaspNotAString = [
      'node',
      'run.js',
      1,
      'output.json',
      JSON.stringify(['entity1', 'entity2']),
    ] as string[]
    const pathToOutputFileNotAString = [
      'node',
      'run.js',
      'main.wasp.js',
      2,
      JSON.stringify(['entity1', 'entity2']),
    ] as string[]
    const entitiesNotAString = [
      'node',
      'run.js',
      'main.wasp.js',
      'output.json',
      ['entity1', 'entity2'],
    ] as string[]

    expect(() =>
      parseProcessArgsOrThrow(pathToMainWaspNotAString)
    ).toThrowError()
    expect(() =>
      parseProcessArgsOrThrow(pathToOutputFileNotAString)
    ).toThrowError()
    expect(() => parseProcessArgsOrThrow(entitiesNotAString)).toThrowError()
  })

  test('should throw an error if the entity names JSON is not an array', () => {
    const entitiesValidJSONButNotArray = [
      ...noEnttiyArgs,
      JSON.stringify({ entity1: 'entity1' }),
    ]

    expect(() =>
      parseProcessArgsOrThrow(entitiesValidJSONButNotArray)
    ).toThrowError()
  })
})
