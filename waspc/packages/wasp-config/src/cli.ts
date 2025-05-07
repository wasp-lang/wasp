import { writeFileSync } from 'fs'
import { analyzeUserApp } from './appAnalyzer.js'

/**
 * Main function that processes command line arguments, analyzes the user app,
 * and writes the output to a file.
 */
export async function main(args: string[]): Promise<void> {
  const { mainWaspJs, declsJsonOutputFile, entityNames } =
    parseProcessArgsOrThrow(args)

  const declsJsonResult = await analyzeUserApp(mainWaspJs, entityNames)

  if (declsJsonResult.status === 'error') {
    console.error(declsJsonResult.error)
    process.exit(1)
  }

  writeFileSync(declsJsonOutputFile, declsJsonResult.value)
}

export function parseProcessArgsOrThrow(args: string[]): {
  mainWaspJs: string
  declsJsonOutputFile: string
  entityNames: string[]
} {
  if (args.length !== 5) {
    throw new Error(
      'Usage: node run.js <path to main.wasp.js> <path to output file> <entity names json>'
    )
  }

  const [_node, _runjs, mainWaspJs, declsJsonOutputFile, entityNamesJson] = args
  if (
    typeof mainWaspJs !== 'string' ||
    typeof declsJsonOutputFile !== 'string' ||
    typeof entityNamesJson !== 'string'
  ) {
    throw new Error(
      'Usage: node run.js <path to main.wasp.js> <path to output file> <entity names json>'
    )
  }

  const entityNames = getValidEntityNamesOrThrow(entityNamesJson)

  return {
    mainWaspJs,
    declsJsonOutputFile,
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
