/**
 * The package managers the docs show commands for. Authors write a command
 * for the first one, npm, and `toPackageManagerCommands.ts` turns it into the
 * command of the others. Adding one here means teaching it there.
 */
export const PACKAGE_MANAGERS = [
  { name: "npm", label: "npm" },
  { name: "pnpm", label: "pnpm" },
  { name: "bun", label: "Bun" },
] as const;

export type PackageManagerName = (typeof PACKAGE_MANAGERS)[number]["name"];

/** A command written out for each package manager. */
export type PackageManagerCommands = Record<PackageManagerName, string>;

/** The tab group that remembers the reader's package manager. */
export const PACKAGE_MANAGER_TAB_GROUP_ID = "package-manager";
