import { $ } from "zx";

import { program } from "@commander-js/extra-typings";
import { extractCommitsCommand } from "./commands/extract-commits";
import { generateAppCommand } from "./commands/generate-app";

const _args = program
  .addCommand(generateAppCommand)
  .addCommand(extractCommitsCommand)
  .parse(process.argv)
  .opts();

$.verbose = true;
