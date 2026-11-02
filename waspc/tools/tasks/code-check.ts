#!/usr/bin/env node
//MISE description="Checks code by running it through formatter, linter and static analysis."
//MISE depends=["workspace-install"]

import { execa } from "execa";
import { styleText } from "node:util";

const CHECKS: [checkName: string, task: string][] = [
  ["Formatter (prettier)", "check:prettier"],
  ["Formatter (ormolu)", "check:ormolu"],
  ["Formatter (cabal-gild)", "check:cabal"],
  ["Linter (hlint)", "hlint"],
  ["Static analysis (stan)", "stan"],
];

const summaryLines: string[] = [];
let allPassed = true;
for (const [checkName, task] of CHECKS) {
  const { exitCode } = await execa("mise", ["run", task], {
    stdio: "inherit",
    reject: false,
  });
  const passed = exitCode === 0;
  allPassed &&= passed;
  summaryLines.push(`${checkName}: ${formatCheckStatus(passed)}`);
}

console.log("\n");
console.log("======================================");
console.log("               SUMMARY");
console.log("======================================");
console.log();
console.log(summaryLines.join("\n"));
console.log("-----------------------");
console.log(`All together: ${formatCheckStatus(allPassed)}`);
process.exitCode = allPassed ? 0 : 1;

function formatCheckStatus(passed: boolean): string {
  return passed ? styleText("green", "OK") : styleText("red", "FAIL");
}
