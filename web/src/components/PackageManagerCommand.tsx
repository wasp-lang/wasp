import CodeBlock from "@theme/CodeBlock";
import TabItem from "@theme/TabItem";
import Tabs from "@theme/Tabs";
import { type ReactNode } from "react";
import {
  assertHasEveryPackageManagerCommand,
  PACKAGE_MANAGER_TAB_GROUP_ID,
  PACKAGE_MANAGERS,
  type PackageManagerCommands,
} from "./packageManagers";

/**
 * Shows a shell command in the reader's preferred package manager, e.g.
 * `<PackageManagerCommand npm="npm install" />`. Write a multi-line command
 * as a string with `\n`.
 */
export default function PackageManagerCommand(
  commands: PackageManagerCommands,
): ReactNode {
  assertHasEveryPackageManagerCommand("PackageManagerCommand", commands);

  if (PACKAGE_MANAGERS.length === 1) {
    return <ShellCodeBlock command={commands[PACKAGE_MANAGERS[0].name]} />;
  }
  return (
    <Tabs groupId={PACKAGE_MANAGER_TAB_GROUP_ID}>
      {PACKAGE_MANAGERS.map(({ name, label }) => (
        <TabItem key={name} value={name} label={label}>
          <ShellCodeBlock command={commands[name]} />
        </TabItem>
      ))}
    </Tabs>
  );
}

function ShellCodeBlock({ command }: { command: string }): ReactNode {
  return <CodeBlock language="shell">{command.trim()}</CodeBlock>;
}
