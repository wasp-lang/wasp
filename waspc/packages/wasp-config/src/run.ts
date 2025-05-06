#!/usr/bin/env node
import { executeMain } from './cli.js'

executeMain(process.argv).catch((error) => {
  console.error(error)
  process.exit(1)
})
