import { program } from "@commander-js/extra-typings";

import { editActionCommand } from "./commands/edit-action";
import { generateAppCommand } from "./commands/generate-app";
import { listActionsCommand } from "./commands/list-actions";

program
  .addCommand(generateAppCommand)
  .addCommand(editActionCommand)
  .addCommand(listActionsCommand)
  .parse(process.argv)
  .opts();
