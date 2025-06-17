import { Command } from "commander";

export function getCommandName(command: Command): string {
  return parseCommandNameFromHelpOutput(command.helpInformation());
}

function parseCommandNameFromHelpOutput(usage: string): string {
  return usage
    .split(/[\r\n]+/)[0]
    .replace("Usage: ", "")
    .replace(" [options]", "");
}
