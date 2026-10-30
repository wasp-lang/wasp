#!/usr/bin/env node
import { writeFileSync } from "fs";
import { parseProcessArgsOrThrow } from "./cli.js";
import { analyzeApp } from "./spec/appAnalyzer.js";
import { WaspSpecUserError } from "./spec/waspSpecUserError.js";
await main(process.argv);
async function main(args) {
    const { waspTsSpecPath, tsconfigPath, projectRootDir, specResultPath, entityNames, } = parseProcessArgsOrThrow(args);
    const result = await analyze({
        waspTsSpecPath,
        tsconfigPath,
        projectRootDir,
        entityNames,
    });
    writeFileSync(specResultPath, JSON.stringify(result));
}
async function analyze(args) {
    try {
        const decls = await analyzeApp(args);
        return { status: "ok", value: decls };
    }
    catch (error) {
        if (error instanceof WaspSpecUserError) {
            return { status: "error", error: error.message };
        }
        throw error;
    }
}
