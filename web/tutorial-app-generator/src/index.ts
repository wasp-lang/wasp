import { program } from "@commander-js/extra-typings";
import { editStepCommand } from "./commands/edit-step";
import { generateAppCommand } from "./commands/generate-app";
import { listStepsCommand } from "./commands/list-steps";

program
  .addCommand(generateAppCommand)
  .addCommand(editStepCommand)
  .addCommand(listStepsCommand)
  .parse(process.argv)
  .opts();
