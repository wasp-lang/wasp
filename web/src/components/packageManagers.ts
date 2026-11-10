/**
 * The package managers the docs show commands for. Adding one here makes its
 * prop required in `PackageManagerCommand` and `WaspCommand`, so the docs build
 * fails until every command has its form for it.
 */
export const PACKAGE_MANAGERS = [{ name: "npm", label: "npm" }] as const;

export type PackageManagerName = (typeof PACKAGE_MANAGERS)[number]["name"];

/** A command written out for each package manager. */
export type PackageManagerCommands = Record<PackageManagerName, string>;

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
        `${componentName} is missing the ${name} command in: ${JSON.stringify(commands)}`,
      );
    }
  }
}
