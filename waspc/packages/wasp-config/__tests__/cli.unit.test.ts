import { describe, expect, test } from 'vitest'
import { getValidEntityNamesOrThrow, parseProcessArgsOrThrow } from '../src/cli'

describe('parseProcessArgsOrThrow', () => {
  test('should parse arguments correctly', () => {
    testParseProcessArgsOrThrow([
      'node',
      'run.js',
      'main.wasp.js',
      'output.json',
      JSON.stringify(['entity1']),
    ])
  })

  test('should parse 0 entities correctly', () => {
    testParseProcessArgsOrThrow([
      'node',
      'run.js',
      'main.wasp.js',
      'output.json',
      '[]',
    ])
  })

  test('should throw an error if less than 5 arguments', () => {
    testParseProcessArgsOrThrow(
      ['node', 'run.js', 'main.wasp.js', 'output.json'],
      { shouldError: true }
    )
  })

  test('should throw an error if more than 5 arguments', () => {
    testParseProcessArgsOrThrow(
      ['node', 'run.js', 'main.wasp.js', 'output.json', '[]', 'extraArg'],
      { shouldError: true }
    )
  })

  test('should throw an error if any of the arguments is not a string', () => {
    testParseProcessArgsOrThrow(
      [
        'node',
        'run.js',
        1,
        'output.json',
        JSON.stringify(['entity1']),
      ] as string[],
      { shouldError: true }
    )
    testParseProcessArgsOrThrow(
      [
        'node',
        'run.js',
        'main.wasp.js',
        2,
        JSON.stringify(['entity1']),
      ] as string[],
      { shouldError: true }
    )
    testParseProcessArgsOrThrow(
      [
        'node',
        'run.js',
        'main.wasp.js',
        'output.json',
        ['entitiy1'],
      ] as string[],
      { shouldError: true }
    )
  })

  test('should throw an error if the entity names JSON is not an array', () => {
    testParseProcessArgsOrThrow(
      [
        'node',
        'run.js',
        'main.wasp.js',
        'output.json',
        JSON.stringify({ entity1: 'entity1' }),
      ],
      { shouldError: true }
    )
  })

  function testParseProcessArgsOrThrow(
    args: string[],
    options:
      | {
          shouldError?: boolean
        }
      | undefined = {
      shouldError: false,
    }
  ) {
    const { shouldError } = options

    if (shouldError) {
      expect(() => parseProcessArgsOrThrow(args)).toThrowError()
      return
    }

    const result = parseProcessArgsOrThrow(args)

    expect(result).toEqual({
      waspTsSpecPath: args.at(2),
      outputFilePath: args.at(3),
      entityNames: args.at(4) && getValidEntityNamesOrThrow(args[4]),
    })
  }
})
