import { useStorageSlot } from "@docusaurus/theme-common";
import { type ReactNode } from "react";
import {
  assertHasEveryPackageManagerCommand,
  PACKAGE_MANAGER_TAB_GROUP_ID,
  PACKAGE_MANAGERS,
  type PackageManagerName,
} from "./packageManagers";
import {
  assertHasGlobalCommand,
  GLOBAL_CLI_TAB_VALUE,
  WASP_CLI_TAB_GROUP_ID,
  type WaspCommandProps,
} from "./WaspCommand";

/**
 * Shows a Wasp command inside a sentence, in the form the reader picked in a
 * `WaspCommand` block, e.g.
 * `<InlineWaspCommand npm="npm run dev" global="wasp start" />`.
 *
 * The server renders the package manager form, so that's what readers see
 * before they pick a tab, and what the Markdown version of the docs has.
 */
export default function InlineWaspCommand({
  global,
  ...packageManagerCommands
}: WaspCommandProps): ReactNode {
  assertHasGlobalCommand("InlineWaspCommand", {
    global,
    ...packageManagerCommands,
  });
  assertHasEveryPackageManagerCommand(
    "InlineWaspCommand",
    packageManagerCommands,
  );

  const [waspCliTab] = useStorageSlot(getTabStorageKey(WASP_CLI_TAB_GROUP_ID));
  const [packageManagerTab] = useStorageSlot(
    getTabStorageKey(PACKAGE_MANAGER_TAB_GROUP_ID),
  );

  const command =
    waspCliTab === GLOBAL_CLI_TAB_VALUE
      ? global
      : packageManagerCommands[getPackageManagerName(packageManagerTab)];

  return <code>{command}</code>;
}

/** Where Docusaurus keeps the tab a reader picked in a tab group. */
function getTabStorageKey(tabGroupId: string): string {
  return `docusaurus.tab.${tabGroupId}`;
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
