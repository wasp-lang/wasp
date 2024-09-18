#!/usr/bin/env node
import { writeFileSync } from 'fs'
import { App, Spec } from './lib.js'

async function main() {
  if (process.argv.length < 4) {
    console.error(
      'Usage: node run.js <path to main.wasp.js> <path to output file>'
    )
    process.exit(1)
  }

  const [_node, _runjs, mainWaspTs, outputFile] = process.argv
  if (typeof mainWaspTs !== 'string' || typeof outputFile !== 'string') {
    console.error(
      'Usage: node run.js <path to main.wasp.js> <path to output file>'
    )
    process.exit(1)
  }

  const app: App = (await import(mainWaspTs)).default
  writeFileSync(outputFile, serialize(app.getSpec()))
  console.log(`Wasp spec written to ${outputFile}`)
}

function serialize(appConfig: Spec): string {
  return JSON.stringify(appConfig)
}

// take an argument from the command line and pass it to main
main()
