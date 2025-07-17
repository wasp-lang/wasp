import { Command } from "@commander-js/extra-typings";

import type { Action } from "../../executeSteps/actions";
import { getActionsFromTutorialFiles } from "../../extractSteps";
import { log } from "../../log";

export const extractCommitsCommand = new Command("extract-commits")
  .description("Extract commits into patch files for the tutorial app")
  .action(async () => {
    const actions: Action[] = await getActionsFromTutorialFiles();

    // TODO: implement command that extracts commits into patch files

    log("success", "Commits successfully extracted into patch files!");
  });
