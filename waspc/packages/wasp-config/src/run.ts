#!/usr/bin/env node
import { writeFileSync } from 'fs'
import { mapUserSpecAppToDeclJson } from './mapUserSpecToAppSepcJson.js'
import { App } from './userApi.js'

main()

async function main() {
  const {
    mainWaspJs,
    outputFile: declsJsonOutputFile,
    entityNames,
  } = parseProcessArgsOrThrow(process.argv)

  const result = await getAppDefinitionOrError(mainWaspJs)
  if (result.status === 'error') {
    console.error(result.error)
    process.exit(1)
  }
  const { value: appDefinition } = result

  const declsJson = mapUserSpecAppToDeclJson(appDefinition, entityNames)

  writeFileSync(declsJsonOutputFile, declsJson)
}

async function getAppDefinitionOrError(
  mainWaspJs: string
): Promise<Result<App, string>> {
  const usersDefaultExport: unknown = (await import(mainWaspJs)).default
  return getValidAppOrError(usersDefaultExport)
}

function getValidAppOrError(app: unknown): Result<App, string> {
  if (!app) {
    return {
      status: 'error',
      error:
        'Could not load your app config. ' +
        'Make sure your *.wasp.ts file includes a default export of the app.',
    }
  }

  if (!(app instanceof App)) {
    return {
      status: 'error',
      error:
        'The default export of your *.wasp.ts file must be an instance of App. ' +
        'Make sure you export an object created with new App(...).',
    }
  }

  return { status: 'ok', value: app }
}

function parseProcessArgsOrThrow(args: string[]): {
  mainWaspJs: string
  outputFile: string
  entityNames: string[]
} {
  if (args.length < 5) {
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

  const entityNames = getValidEntityNamesOrThrow(entityNamesJson)

  return {
    mainWaspJs,
    outputFile,
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

type Result<Value, Error> =
  | { status: 'ok'; value: Value }
  | { status: 'error'; error: Error }
