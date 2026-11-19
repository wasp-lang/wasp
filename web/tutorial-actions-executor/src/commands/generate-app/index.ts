import type { Action } from "../../actions/actions.ts";
import { getActionsFromTutorialFiles } from "../../extract-actions/index.ts";
import { log } from "../../log.ts";
import { createTutorialApp, type TutorialApp } from "../../tutorialApp.ts";
import type { WaspCliCommand } from "../../waspCli.ts";
import { waspCliCommandOption } from "../commonOptions.ts";
import { createTacteCommand } from "../tacteCommand.ts";
import { executeActions } from "./execute-actions.ts";

export const generateAppCommand = createTacteCommand("generate-app")
  .description("Generate a new Wasp app based on the tutorial actions")
  .addOption(waspCliCommandOption)
  .action(async ({ waspCliCommand, appName, outputDir, tutorialDir }) => {
    const tutorialApp = createTutorialApp({
      appName,
      outputDir,
      tutorialDir,
    });

    const actions = await getActionsFromTutorialFiles(tutorialApp);
    log("info", `Found ${actions.length} actions in tutorial files.`);

    await generateApp(actions, waspCliCommand, tutorialApp);
  });

export async function generateApp(
  actions: Action[],
  waspCliCommand: WaspCliCommand,
  tutorialApp: TutorialApp,
): Promise<void> {
  await executeActions({
    waspCliCommand,
    tutorialApp,
    actions,
  });
  log(
    "success",
    `Tutorial app has been successfully generated in ${tutorialApp.appDirPath}`,
  );
}
