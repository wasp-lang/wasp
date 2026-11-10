import CodeBlock from "@theme/CodeBlock";
import TabItem from "@theme/TabItem";
import Tabs from "@theme/Tabs";
import { type ReactNode } from "react";
import PackageManagerCommand from "./PackageManagerCommand";
import {
  PACKAGE_MANAGERS,
  type PackageManagerCommands,
} from "./packageManagers";

export type WaspCommandProps = PackageManagerCommands & {
  /** The same command with the global Wasp CLI. */
  global: string;
};

/** The tab group that remembers how the reader runs Wasp. */
export const WASP_CLI_TAB_GROUP_ID = "wasp-cli";
export const PACKAGE_MANAGER_TAB_VALUE = "package-manager";
export const GLOBAL_CLI_TAB_VALUE = "global";

/**
 * Shows a Wasp command the way the reader prefers to run Wasp: through their
 * package manager, or with the global Wasp CLI, e.g.
 * `<WaspCommand npm="npm run dev" global="wasp start" />`. Write a multi-line
 * command as a string with `\n`.
 */
export default function WaspCommand({
  global,
  ...packageManagerCommands
}: WaspCommandProps): ReactNode {
  assertHasGlobalCommand("WaspCommand", { global, ...packageManagerCommands });

  return (
    <Tabs groupId={WASP_CLI_TAB_GROUP_ID}>
      <TabItem
        value={PACKAGE_MANAGER_TAB_VALUE}
        label={PACKAGE_MANAGER_TAB_LABEL}
      >
        <PackageManagerCommand {...packageManagerCommands} />
      </TabItem>
      <TabItem value={GLOBAL_CLI_TAB_VALUE} label="Global CLI">
        <CodeBlock language="shell">{global.trim()}</CodeBlock>
      </TabItem>
    </Tabs>
  );
}

/** MDX props aren't type-checked, so a missing command only shows up here. */
export function assertHasGlobalCommand(
  componentName: string,
  commands: WaspCommandProps,
): void {
  if (!commands.global) {
    throw new Error(
      `${componentName} is missing the global command in: ${JSON.stringify(commands)}`,
    );
  }
}

const PACKAGE_MANAGER_TAB_LABEL =
  PACKAGE_MANAGERS.length === 1 ? PACKAGE_MANAGERS[0].label : "Package manager";
