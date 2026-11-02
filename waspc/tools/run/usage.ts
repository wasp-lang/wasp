/// <reference types="node" />

import { styleText } from "node:util";
import type { Command } from "./commands.ts";

const DESCRIPTION_WIDTH = 80;
const COMMAND_INDENT = "    ";
const DESCRIPTION_INDENT = "        ";

export function printUsage(commandGroups: Command[][]): void {
  const commandsUsage = commandGroups
    .map((commandGroup) => commandGroup.flatMap(formatCommandUsage).join("\n"))
    .join("\n\n");
  const usage = [
    styleText("bold", "USAGE"),
    `${COMMAND_INDENT}run <command>`,
    "",
    styleText("bold", "COMMANDS"),
    commandsUsage,
  ];
  console.log(usage.join("\n"));
}

function formatCommandUsage(command: Command): string[] {
  const signature = [command.name, command.argsHint].filter(Boolean).join(" ");
  return [
    COMMAND_INDENT + styleText(["yellow", "bold"], signature),
    ...wrapWords(command.description, DESCRIPTION_WIDTH).map(
      (line) => DESCRIPTION_INDENT + line,
    ),
  ];
}

function wrapWords(text: string, width: number): string[] {
  const lines: string[] = [];
  let currentLine = "";
  for (const word of text.split(" ")) {
    const candidateLine = currentLine ? `${currentLine} ${word}` : word;
    if (candidateLine.length > width && currentLine) {
      lines.push(currentLine);
      currentLine = word;
    } else {
      currentLine = candidateLine;
    }
  }
  lines.push(currentLine);
  return lines;
}
