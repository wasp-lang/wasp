import { useStorageSlot } from "@docusaurus/theme-common";
import { type ReactNode } from "react";
import {
  PACKAGE_MANAGER_TAB_GROUP_ID,
  PACKAGE_MANAGERS,
  type PackageManagerName,
} from "./packageManagers";
import {
  toPackageManagerCommands,
  type WaspCli,
} from "./toPackageManagerCommands";
import {
  assertRunsWasp,
  WASP_CLI_TAB_GROUP_ID,
  type WaspCommandProps,
} from "./WaspCommand";

/**
 * Shows a Wasp command inside a sentence, in the form the reader picked in a
 * `WaspCommand` block, e.g.
 * `<InlineWaspCommand command="wasp start" />`.
 *
 * The server renders the package manager form, so that's what readers see
 * before they pick a tab, and what the Markdown version of the docs has.
 */
export default function InlineWaspCommand({
  command,
}: WaspCommandProps): ReactNode {
  assertRunsWasp("InlineWaspCommand", command);

  const [waspCliTab] = useStorageSlot(getTabStorageKey(WASP_CLI_TAB_GROUP_ID));
  const [packageManagerTab] = useStorageSlot(
    getTabStorageKey(PACKAGE_MANAGER_TAB_GROUP_ID),
  );

  const commands = toPackageManagerCommands(command, getWaspCli(waspCliTab));

  return <code>{commands[getPackageManagerName(packageManagerTab)]}</code>;
}

/** Where Docusaurus keeps the tab a reader picked in a tab group. */
function getTabStorageKey(tabGroupId: string): string {
  return `docusaurus.tab.${tabGroupId}`;
}

/** Falls back to the project's Wasp until the reader picks one. */
function getWaspCli(waspCliTab: string | null): WaspCli {
  return waspCliTab === "global" ? "global" : "project";
}

/** Falls back to the first package manager until the reader picks one. */
function getPackageManagerName(
  packageManagerTab: string | null,
): PackageManagerName {
  const pickedPackageManager = PACKAGE_MANAGERS.find(
    ({ name }) => name === packageManagerTab,
  );
  return (pickedPackageManager ?? PACKAGE_MANAGERS[0]).name;
}
