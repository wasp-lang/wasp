import type { Action } from "../../actions/actions";
import { getActionsFromTutorialFiles } from "../../extract-actions";
import { log } from "../../log";
import { createTutorialApp, type TutorialApp } from "../../tutorialApp";
import type { WaspCliCommand } from "../../waspCli";
import { waspCliCommandOption } from "../commonOptions";
import { createTacteCommand } from "../tacteCommand";
import { executeActions } from "./execute-actions";

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
