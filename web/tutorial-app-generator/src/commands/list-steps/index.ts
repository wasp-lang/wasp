import { Command } from "@commander-js/extra-typings";
import { basename } from "path";
import { chalk } from "zx";

import type { Action } from "../../executeSteps/actions";
import { getActionsFromTutorialFiles } from "../../extractSteps";
import { tutorialDir } from "../../project";

type ActionsGroupedByFile = Map<string, Action[]>;

export const listStepsCommand = new Command("list-steps")
  .description("List all steps in the tutorial")
  .action(async () => {
    const actions = await getActionsFromTutorialFiles(tutorialDir);
    const actionsGroupedByFile = groupActionsBySourceFile(actions);
    displayGroupedActions(actionsGroupedByFile);
  });

function groupActionsBySourceFile(actions: Action[]): ActionsGroupedByFile {
  const groupedActions = new Map<string, Action[]>();

  for (const action of actions) {
    const filename = basename(action.sourceFilePath);
    const existingActions = groupedActions.get(filename) ?? [];
    groupedActions.set(filename, [...existingActions, action]);
  }

  return groupedActions;
}

function displayGroupedActions(
  actionsGroupedByFile: ActionsGroupedByFile,
): void {
  for (const [filename, fileActions] of actionsGroupedByFile) {
    displayFileHeader(filename);
    displayActionsForFile(fileActions);

    console.log();
  }
}

function displayFileHeader(filename: string): void {
  console.log(chalk.bold.magenta(filename));
  console.log();
}

function displayActionsForFile(actions: Action[]): void {
  type ActionKind = Action["kind"];
  const kindConfig: Record<ActionKind, { color: (text: string) => string }> = {
    "apply-patch": { color: chalk.green },
    "migrate-db": { color: chalk.blue },
  };

  actions.forEach((action) => {
    const config = kindConfig[action.kind];
    const coloredKind = config.color(action.kind);

    console.log(
      `- ${chalk.bold(action.id)} ${chalk.gray("(")}${coloredKind}${chalk.gray(")")}`,
    );
  });
}
