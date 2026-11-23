import { type ReactNode } from "react";

/**
 * The package managers the docs show commands for. Adding one makes its prop
 * required in `PackageManagerCommand`, so the docs build fails until every
 * command has a form for it. `toPackageManagerCommands` needs to learn it too,
 * since it writes the commands of `WaspCommand`.
 */
export const PACKAGE_MANAGERS = [{ name: "npm", label: "npm" }] as const;

export type PackageManagerName = (typeof PACKAGE_MANAGERS)[number]["name"];

/** A command written out for each package manager. */
export type PackageManagerCommands = Record<PackageManagerName, string>;

/**
 * Reads a command passed as children, e.g.
 * `<WaspCommand>{`wasp start`}</WaspCommand>`. MDX parses plain text children
 * as Markdown, so only a template literal works. MDX also drops the leading
 * spaces of each line, so the lines can't be indented.
 */
export function getCommandFromChildren(
  componentName: string,
  children: ReactNode,
): string | undefined {
  if (children === undefined) {
    return undefined;
  }
  if (typeof children !== "string") {
    throw new Error(
      `${componentName} needs its command as a template literal, e.g. {\`wasp start\`}.`,
    );
  }
  return children.trim();
}

/** The tab group that remembers the reader's package manager. */
export const PACKAGE_MANAGER_TAB_GROUP_ID = "package-manager";

/** MDX props aren't type-checked, so a missing command only shows up here. */
export function assertHasEveryPackageManagerCommand(
  componentName: string,
  commands: PackageManagerCommands,
): void {
  for (const { name } of PACKAGE_MANAGERS) {
    if (!commands[name]) {
      throw new Error(
        `${componentName} is missing the ${name} command in ${JSON.stringify(commands)}.`,
      );
    }
  }
}
