import { Command } from "@commander-js/extra-typings";
import { basename } from "path";
import { chalk } from "zx";

import type { Action } from "../../actions/actions";
import { getActionsFromTutorialFiles } from "../../extract-actions";
import { docsTutorialDirPath } from "../../tutorialApp";

type ActionsGroupedByFile = Map<string, Action[]>;

export const listActionsCommand = new Command("list-actions")
  .description("List all actions in the tutorial")
  .action(async () => {
    const actions = await getActionsFromTutorialFiles(docsTutorialDirPath);
    const actionsGroupedByFile = groupActionsBySourceFile(actions);
    displayGroupedActions(actionsGroupedByFile);
  });

function groupActionsBySourceFile(actions: Action[]): ActionsGroupedByFile {
  const groupedActions = new Map<string, Action[]>();

  for (const action of actions) {
    const filename = basename(action.tutorialFilePath);
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
  const kindColorMap: Record<Action["kind"], (text: string) => string> = {
    INIT_APP: chalk.yellow,
    APPLY_PATCH: chalk.green,
    MIGRATE_DB: chalk.blue,
  };

  actions.forEach((action) => {
    const kindColorFn = kindColorMap[action.kind];

    console.log(`- ${chalk.bold(action.id)} (${kindColorFn(action.kind)})`);
  });
}
