import { $ } from "zx";

import { program } from "@commander-js/extra-typings";
import type { Action } from "./executeSteps/actions";
import { executeSteps } from "./executeSteps/index";
import { getActionsFromTutorialFiles } from "./extractSteps/index";
import { appDir } from "./paths";
import { waspNew } from "./waspCli";

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
