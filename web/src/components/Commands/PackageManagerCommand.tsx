import CodeBlock from "@theme/CodeBlock";
import TabItem from "@theme/TabItem";
import Tabs from "@theme/Tabs";
import { type ReactNode } from "react";
import {
  assertHasEveryPackageManagerCommand,
  getCommandFromChildren,
  PACKAGE_MANAGER_TAB_GROUP_ID,
  PACKAGE_MANAGERS,
  type PackageManagerCommands,
  type PackageManagerName,
} from "./packageManagers";

/** Gets the command either as props or as children. */
export type PackageManagerCommandProps =
  | (PackageManagerCommands & { children?: never })
  | (Partial<Record<PackageManagerName, never>> & { children: string });

/**
 * Shows a shell command in the reader's package manager, e.g.
 * `<PackageManagerCommand npm="npm install" />`. A command with several lines
 * reads better as children:
 *
 * ```mdx
 * <PackageManagerCommand>{`
 * npm install multer
 * npm install --save-dev @types/multer
 * `}</PackageManagerCommand>
 * ```
 */
export default function PackageManagerCommand({
  children,
  ...commandProps
}: PackageManagerCommandProps): ReactNode {
  const commands = {
    npm: getCommandFromChildren("PackageManagerCommand", children),
    ...commandProps,
  } as PackageManagerCommands;
  assertHasEveryPackageManagerCommand("PackageManagerCommand", commands);

  const distinctCommands = new Set(
    PACKAGE_MANAGERS.map(({ name }) => commands[name]),
  );
  if (distinctCommands.size === 1) {
    // Every package manager runs the same command, so there's nothing to pick.
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
