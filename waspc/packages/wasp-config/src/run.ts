#!/usr/bin/env node

import { writeFileSync } from 'fs'
import { analyzeUserApp } from './appAnalyzer.js'
import { parseProcessArgsOrThrow } from './cli.js'

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

main(process.argv).catch((error) => {
  console.error(error)
  process.exit(1)
})
