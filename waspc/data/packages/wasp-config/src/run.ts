#!/usr/bin/env node

import { readFileSync, writeFileSync } from "fs";
import { analyzeApp } from "./appAnalyzer.js";
import { analyzeModule } from "./moduleAnalyzer.js";
import { parseProcessArgsOrThrow } from "./cli.js";
import { Module } from "./publicApi/Module.js";
import { App } from "./publicApi/App.js";

main(process.argv);

async function main(args: string[]): Promise<void> {
  const { waspTsSpecPath, outputFilePath, entityNames } =
    parseProcessArgsOrThrow(args);

  const pkgJson = JSON.parse(readFileSync("package.json", "utf-8"));

  // Set projectPackageName BEFORE importing the user's file so that
  // app.use() can correctly detect self-referencing modules (where
  // packageName === projectPackageName) and skip path rewriting.
  App.projectPackageName = pkgJson.name;

  const defaultExport: unknown = (await import(waspTsSpecPath)).default;

  if (defaultExport instanceof Module) {
    const result = await analyzeModule(defaultExport);
    if (result.status === "error") {
      console.error(result.error);
      process.exit(1);
    }
    writeFileSync(
      outputFilePath,
      JSON.stringify({ mode: "module", spec: result.value }),
    );
  } else if (defaultExport instanceof App) {
    const declsResult = await analyzeApp(waspTsSpecPath, entityNames, pkgJson.name);
    if (declsResult.status === "error") {
      console.error(declsResult.error);
      process.exit(1);
    }
    // App mode outputs a raw declarations array for backward compatibility
    // with the Haskell parser (which expects [Decl]).
    writeFileSync(outputFilePath, JSON.stringify(declsResult.value));
  } else {
    console.error(
      "Default export must be an App or Module instance. " +
        "Make sure your *.wasp.ts file exports an object created with new App(...) or new Module(...).",
    );
    process.exit(1);
  }
}
