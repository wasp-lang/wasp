#!/usr/bin/env node
import { writeFileSync } from 'fs'
import { App, Spec } from './lib.js'

async function main() {
  const {
    mainWaspJs: mainWaspTs,
    outputFile,
    entityNames,
  } = parseProcessArguments(process.argv)

  const app: App = (await import(mainWaspTs)).default
  const spec = analyzeApp(app, entityNames)

  writeFileSync(outputFile, serialize(spec))
  console.log(`Wasp spec written to ${outputFile}`)
}

function analyzeApp(app: App, entityNames: string[]): Spec {
  return app.getSpec()
}

function parseProcessArguments(args: string[]): {
  mainWaspJs: string
  outputFile: string
  entityNames: string[]
} {
  if (process.argv.length < 5) {
    throw new Error(
      'Usage: node run.js <path to main.wasp.js> <path to output file>'
    )
  }

  const [_node, _runjs, mainWaspJs, outputFile, entityNamesJson] = process.argv
  if (
    typeof mainWaspJs !== 'string' ||
    typeof outputFile !== 'string' ||
    typeof entityNamesJson !== 'string'
  ) {
    throw new Error(
      'Usage: node run.js <path to main.wasp.js> <path to output file> <entity names json>'
    )
  }

  const entityNames = parseEntityNamesJson(entityNamesJson)

  return {
    mainWaspJs,
    outputFile,
    entityNames,
  }
}

function parseEntityNamesJson(entitiesJson: string): string[] {
  const entities = JSON.parse(entitiesJson)
  if (!Array.isArray(entities)) {
    throw new Error('The entities JSON must be an array of entity names.')
  }
  return entities
}

function serialize(appConfig: Spec): string {
  return JSON.stringify(appConfig)
}

// take an argument from the command line and pass it to main
main()
