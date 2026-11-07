import CodeBlock from "@theme/CodeBlock";
import TabItem from "@theme/TabItem";
import Tabs from "@theme/Tabs";
import { type ReactNode } from "react";
import PackageManagerCommand from "./PackageManagerCommand";
import {
  PACKAGE_MANAGERS,
  type PackageManagerCommands,
} from "./packageManagers";

type Props = PackageManagerCommands & {
  /** The same command with the global Wasp CLI. */
  global: string;
};

/**
 * Shows a Wasp command the way the reader prefers to run Wasp: through their
 * package manager, or with the global Wasp CLI, e.g.
 * `<WaspCommand npm="npm run dev" global="wasp start" />`. Write a multi-line
 * command as a string with `\n`.
 */
export default function WaspCommand({
  global,
  ...packageManagerCommands
}: Props): ReactNode {
  if (!global) {
    throw new Error(
      `WaspCommand is missing the global command in: ${JSON.stringify(packageManagerCommands)}`,
    );
  }

  return (
    <Tabs groupId="wasp-cli">
      <TabItem value="package-manager" label={PACKAGE_MANAGER_TAB_LABEL}>
        <PackageManagerCommand {...packageManagerCommands} />
      </TabItem>
      <TabItem value="global" label="Global CLI">
        <CodeBlock language="shell">{global.trim()}</CodeBlock>
      </TabItem>
    </Tabs>
  );
}

const PACKAGE_MANAGER_TAB_LABEL =
  PACKAGE_MANAGERS.length === 1 ? PACKAGE_MANAGERS[0].label : "Package manager";
