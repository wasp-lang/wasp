import { program } from "@commander-js/extra-typings";
import { editStepCommand } from "./commands/edit-step";
import { generateAppCommand } from "./commands/generate-app";

program
  .addCommand(generateAppCommand)
  .addCommand(editStepCommand)
  .parse(process.argv)
  .opts();
