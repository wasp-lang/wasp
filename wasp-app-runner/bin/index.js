#!/usr/bin/env node

import { main } from "../dist/index.js";

try {
  await main();
} catch (error) {
  console.error(error);
  process.exitCode = 1;
}
