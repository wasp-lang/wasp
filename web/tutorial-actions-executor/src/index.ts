import { program } from "@commander-js/extra-typings";

import { editPatchActionCommand } from "./commands/edit-patch-action";
import { generateAppCommand } from "./commands/generate-app";
import { listActionsCommand } from "./commands/list-actions";

program
  .addCommand(generateAppCommand)
  .addCommand(editPatchActionCommand)
  .addCommand(listActionsCommand)
  .parse(process.argv)
  .opts();
