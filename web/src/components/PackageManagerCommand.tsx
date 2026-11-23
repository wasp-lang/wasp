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
} from "./packageManagers";

export type PackageManagerCommandProps = Partial<PackageManagerCommands> & {
  /** The npm command, as an alternative to the `npm` prop. */
  children?: ReactNode;
};

/**
 * Shows a shell command in the reader's preferred package manager, e.g.
 * `<PackageManagerCommand npm="npm install" />`. A multi-line command reads
 * better as children:
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
  const npmCommandFromChildren = getCommandFromChildren(
    "PackageManagerCommand",
    children,
  );
  if (npmCommandFromChildren !== undefined && commandProps.npm !== undefined) {
    throw new Error(
      "PackageManagerCommand got the npm command both as the `npm` prop and as children.",
    );
  }
  const commands = {
    ...commandProps,
    npm: commandProps.npm ?? npmCommandFromChildren,
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
