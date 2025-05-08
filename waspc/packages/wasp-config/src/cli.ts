import { writeFileSync } from 'fs'
import { analyzeUserApp } from './appAnalyzer.js'

/**
 * Main function that processes command line arguments, analyzes the user app,
 * and writes the output to a file.
 */
export async function main(args: string[]): Promise<void> {
  const { waspTsSpecPath, outputFilePath, entityNames } =
    parseProcessArgsOrThrow(args)

  const declsJsonResult = await analyzeUserApp(waspTsSpecPath, entityNames)

  if (declsJsonResult.status === 'error') {
    console.error(declsJsonResult.error)
    process.exit(1)
  }

  writeFileSync(outputFilePath, declsJsonResult.value)
}

export function parseProcessArgsOrThrow(args: string[]): {
  waspTsSpecPath: string
  outputFilePath: string
  entityNames: string[]
} {
  if (args.length !== 5) {
    throw new Error(
      'Usage: node run.js <path to main.wasp.js> <path to output file> <entity names json>'
    )
  }

  const [_node, _runjs, waspTsSpecPath, outputFilePath, entityNamesJson] = args
  if (
    typeof waspTsSpecPath !== 'string' ||
    typeof outputFilePath !== 'string' ||
    typeof entityNamesJson !== 'string'
  ) {
    throw new Error(
      'Usage: node run.js <path to main.wasp.js> <path to output file> <entity names json>'
    )
  }

  const entityNames = getValidEntityNamesOrThrow(entityNamesJson)

  return {
    waspTsSpecPath,
    outputFilePath,
    entityNames,
  }
}

function getValidEntityNamesOrThrow(entitiesJson: string): string[] {
  const entities = JSON.parse(entitiesJson)
  if (!Array.isArray(entities)) {
    throw new Error('The entities JSON must be an array of entity names.')
  }
  return entities
}
