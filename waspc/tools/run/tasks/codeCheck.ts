/// <reference types="node" />

import { ExecaError } from "execa";
import { styleText } from "node:util";
import { checkCabalGild, checkOrmolu, checkPrettier } from "./formatting.ts";
import { runHlint, runStan } from "./haskell.ts";
import { installWorkspace } from "./nodeTools.ts";

type CheckResult = { checkName: string; passed: boolean };

export async function runCodeCheck(): Promise<void> {
  const checkResults = [
    await runCheck("Formatter (prettier)", async () => {
      await installWorkspace();
      await checkPrettier();
    }),
    await runCheck("Formatter (ormolu)", checkOrmolu),
    await runCheck("Formatter (cabal-gild)", checkCabalGild),
    await runCheck("Linter (hlint)", () => runHlint([])),
    await runCheck("Static analysis (stan)", () => runStan([])),
  ];
  const allPassed = checkResults.every(({ passed }) => passed);

  printSummary(checkResults, { checkName: "All together", passed: allPassed });
  if (!allPassed) {
    process.exitCode = 1;
  }
}

async function runCheck(
  checkName: string,
  check: () => Promise<void>,
): Promise<CheckResult> {
  return { checkName, passed: await passes(check) };
}

// Only a process that ran and exited non-zero counts as a failed check.
// Interrupts and missing tools still stop the whole code check.
async function passes(check: () => Promise<void>): Promise<boolean> {
  try {
    await check();
    return true;
  } catch (error) {
    if (error instanceof ExecaError && error.exitCode !== undefined) {
      return false;
    }
    throw error;
  }
}

function printSummary(
  checkResults: CheckResult[],
  overallResult: CheckResult,
): void {
  console.log("\n");
  console.log("======================================");
  console.log("               SUMMARY");
  console.log("======================================");
  console.log();
  for (const checkResult of checkResults) {
    console.log(formatCheckResult(checkResult));
  }
  console.log("-----------------------");
  console.log(formatCheckResult(overallResult));
}

function formatCheckResult({ checkName, passed }: CheckResult): string {
  const status = passed ? styleText("green", "OK") : styleText("red", "FAIL");
  return `${checkName}: ${status}`;
}
