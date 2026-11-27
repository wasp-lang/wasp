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
import { assertRunsWasp, WASP_CLI_TAB_GROUP_ID } from "./WaspCommand";

/**
 * Shows a Wasp command inside a sentence, in the form the reader picked in a
 * `WaspCommand` block, e.g. `<InlineWaspCommand command="wasp start" />`.
 * The server defaults to the package manager form.
 */
export default function InlineWaspCommand({
  command,
}: {
  /** The command as you'd run it with the global Wasp CLI, e.g. `wasp start`. */
  command: string;
}): ReactNode {
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
