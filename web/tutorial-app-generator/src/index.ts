import { $ } from "zx";

import { program } from "@commander-js/extra-typings";
import type { Action } from "./actions/index";
import { getActionsFromTutorialFiles } from "./markdown/extractSteps";
import { appDir } from "./paths";
import { waspNew } from "./waspCli";

import { executeSteps } from "./execute-steps";

const actions: Action[] = await getActionsFromTutorialFiles();

function findStepOrThrow(stepName: string): Action {
  const action = actions.find((action) => action.step === stepName);
  if (!action) {
    throw new Error(`No action found for step ${stepName}.`);
  }
  return action;
}

const { untilStep } = program
  .option(
    "-s, --until-step <step-name>",
    "Run until the given step. If not provided, run all steps.",
    (stepName: string) => {
      return findStepOrThrow(stepName);
    },
  )
  .parse(process.argv)
  .opts();

$.verbose = true;

async function prepareApp() {
  await $`rm -rf ${appDir}`;
  await waspNew(appDir);
  // Git needs to be initialized for patches to work
  await $`cd ${appDir} && git init && git add . && git commit -m "Initial commit"`;
}

await prepareApp();

await executeSteps(actions, {
  untilStep,
});

// Commit -> patch
// git format-patch -1 migration-connect-task-user --stdout

// git switch -c fixes $step4commitSHA
// git commit -a --fixup=$step4commitSHA
// git switch main
// git rebase fixes
// git rebase --root --autosquash
