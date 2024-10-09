#!/usr/bin/env node
import { writeFileSync } from 'fs'
import { App } from './userApi.js'
import { Decl } from './appSpec.js'
import { mapUserSpecToDecls } from './mappers.js'
import { GET_USER_SPEC } from './_private.js'
import { exit } from 'process'

main()

async function main() {
  const { mainWaspJs, outputFile, entityNames } = parseProcessArguments(
    process.argv
  )

  const app = await importApp(mainWaspJs)
  const spec = analyzeApp(app, entityNames)

  writeFileSync(outputFile, serialize(spec))
}

async function importApp(mainWaspJs: string): Promise<App> {
  const app: unknown = (await import(mainWaspJs)).default
  if (!app) {
    console.error(
      'Could not load your app config. Make sure your *.wasp.ts file includes a default export of the app.'
    )
    exit(1)
  }
  if (!isApp(app)) {
    console.error(
      'The default export of your *.wasp.ts file must be an instance of App.'
    )
    console.error('Make sure you export an object created with new App(...).')
    exit(1)
  }
  return app
}

function isApp(app: unknown): app is App {
  return app instanceof App
}

function analyzeApp(app: App, entityNames: string[]): Decl[] {
  const userSpec = app[GET_USER_SPEC]()
  return mapUserSpecToDecls(userSpec, entityNames)
}

function parseProcessArguments(args: string[]): {
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

function serialize(appConfig: Decl[]): string {
  return JSON.stringify(appConfig)
}
