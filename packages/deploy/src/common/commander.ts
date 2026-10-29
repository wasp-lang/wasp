import { Command } from "commander";

export function getFullCommandName(command: Command): string {
  const fullCommandNameParts = [command.name()];
  for (let parent = command.parent; parent; parent = parent.parent) {
    fullCommandNameParts.unshift(parent.name());
  }
  return fullCommandNameParts.join(" ");
}
