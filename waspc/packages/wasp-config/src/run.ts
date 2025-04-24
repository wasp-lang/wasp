#!/usr/bin/env node
import { writeFileSync } from 'fs'
import { tryAnalyzeUserApp } from './appAnalyzer.js'

main()

async function main() {
  const { mainWaspJs, declsJsonOutputFile, entityNames } =
    parseProcessArgsOrThrow(process.argv)

  const result = await tryAnalyzeUserApp(mainWaspJs, entityNames)

  if (result.status === 'error') {
    console.error(result.error)
    process.exit(1)
  }

  writeFileSync(declsJsonOutputFile, result.value)
}

function parseProcessArgsOrThrow(args: string[]): {
  mainWaspJs: string
  declsJsonOutputFile: string
  entityNames: string[]
} {
  if (args.length < 5) {
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
