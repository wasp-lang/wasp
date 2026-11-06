/**
 * The package managers the docs show commands for. Adding one here makes its
 * prop required in `PackageManagerCommand` and `WaspCommand`, so the docs build
 * fails until every command has its form for it.
 */
export const PACKAGE_MANAGERS = [{ name: "npm", label: "npm" }] as const;

export type PackageManagerName = (typeof PACKAGE_MANAGERS)[number]["name"];

/** A command written out for each package manager. */
export type PackageManagerCommands = Record<PackageManagerName, string>;
