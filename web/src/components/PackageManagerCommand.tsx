import CodeBlock from "@theme/CodeBlock";
import TabItem from "@theme/TabItem";
import Tabs from "@theme/Tabs";
import { type ReactNode } from "react";
import {
  PACKAGE_MANAGER_TAB_GROUP_ID,
  PACKAGE_MANAGERS,
  type PackageManagerCommands,
} from "./packageManagers";
import { fromNpmCommand } from "./toPackageManagerCommands";

/**
 * The command for npm, from which the other package managers' commands follow.
 * Pass another package manager's command only when it isn't what follows.
 */
type PackageManagerCommandProps = Pick<PackageManagerCommands, "npm"> &
  Partial<PackageManagerCommands>;

/**
 * Shows a shell command in the reader's preferred package manager, e.g.
 * `<PackageManagerCommand npm="npm install" />`. Write a multi-line command
 * as a string with `\n`.
 */
export default function PackageManagerCommand(
  props: PackageManagerCommandProps,
): ReactNode {
  // MDX props aren't type-checked, so a missing command only shows up here.
  if (!props.npm) {
    throw new Error(
      `PackageManagerCommand is missing the npm command in: ${JSON.stringify(props)}`,
    );
  }
  const commands: PackageManagerCommands = {
    ...fromNpmCommand(props.npm),
    ...props,
  };

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
