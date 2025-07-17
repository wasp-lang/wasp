import { $ } from "zx";

import { program } from "@commander-js/extra-typings";
import type { Action } from "./actions/index";
import { getActionsFromTutorialFiles } from "./markdown/extractSteps";
import { appDir } from "./paths";
import { waspNew } from "./waspCli";

import { executeSteps } from "./execute-steps";

const actions: Action[] = await getActionsFromTutorialFiles();

function findStepOrThrow(stepName: string): Action {
  const action = actions.find((action) => action.stepName === stepName);
  if (!action) {
    throw new Error(`No action found for step ${stepName}.`);
  }
  return action;
}

const _args = program.parse(process.argv).opts();

$.verbose = true;

async function prepareApp() {
  await $`rm -rf ${appDir}`;
  await waspNew(appDir);
  // Git needs to be initialized for patches to work
  await $`cd ${appDir} && git init && git add . && git commit -m "Initial commit"`;
}

await prepareApp();

await executeSteps(actions);

// Commit -> patch
// git format-patch -1 migration-connect-task-user --stdout

// git switch -c fixes $step4commitSHA
// git commit -a --fixup=$step4commitSHA
// git switch main
// git rebase fixes
// git rebase --root --autosquash

// You can't just change stuff... you need to have the old commits
// ready first - then you execute "change step" and it will regenerate
// the patches for you.

// Do we just use Git for this workflow? Or we wrap the Git workflow
// in this tutorial app generator CLI?
