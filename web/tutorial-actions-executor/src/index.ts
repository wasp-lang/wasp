import { program } from "@commander-js/extra-typings";

import { editPatchActionCommand } from "./commands/edit-patch-action/index.ts";
import { generateAppCommand } from "./commands/generate-app/index.ts";
import { listActionsCommand } from "./commands/list-actions/index.ts";

program
  .addCommand(generateAppCommand)
  .addCommand(editPatchActionCommand)
  .addCommand(listActionsCommand)
  .parse(process.argv)
  .opts();
