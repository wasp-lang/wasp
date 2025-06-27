import { Command, Help } from "commander";

const help = new Help();

export function getCommandName(command: Command): string {
  return help.commandUsage(command).replace(" [options]", "").trim();
}
