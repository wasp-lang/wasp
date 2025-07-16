import path from "path";

import { $ } from "zx";

import { program } from "@commander-js/extra-typings";
import type { Action } from "./actions/index";
import { getActionsFromTutorialFiles } from "./markdown/extractSteps";
import { appDir } from "./paths";
import { waspNew } from "./waspCli";

import { updateBrokenDiffs } from "./edit";
import { executeSteps } from "./execute-steps";

const actions: Action[] = await getActionsFromTutorialFiles();

const { brokenDiff, untilStep } = program
  .option(
    "-s, --until-step <step>",
    "Run until the given step. If not provided, run all steps.",
    (value: string) => {
      const step = parseInt(value, 10);
      if (isNaN(step) || step < 1) {
        throw new Error("Step must be a positive integer.");
      }
      return step;
    },
  )
  .option(
    "-e, --broken-diff <step>",
    "Edit mode, you will edit the diff interactively and all the steps related to the same file that come after.",
    (value: string) => {
      const step = parseInt(value, 10);
      if (isNaN(step) || step < 1) {
        throw new Error("Step must be a positive integer.");
      }
      const actionAtStep = actions.find((action) => action.step === step);
      if (!actionAtStep) {
        throw new Error(`No action found for step ${step}.`);
      }
      if (actionAtStep.kind !== "diff") {
        throw new Error(`Action at step ${step} is not a diff action.`);
      }
      return actionAtStep;
    },
  )
  .parse(process.argv)
  .opts();

$.verbose = true;

async function prepareApp() {
  await $`rm -rf ${appDir}`;
  await waspNew(appDir);
  // TODO: Maybe we should have a whitelist of files we want to keep in src?
  await $`rm ${path.join(appDir, "src/Main.css")}`;
  await $`rm ${path.join(appDir, "src/waspLogo.png")}`;
  await $`rm ${path.join(appDir, "src/MainPage.jsx")}`;
  // Git needs to be initialized for patches to work
  await $`cd ${appDir} && git init`;
}

await prepareApp();

if (brokenDiff) {
  await updateBrokenDiffs(brokenDiff, actions);
} else {
  await executeSteps(actions, {
    untilStep,
  });
}
