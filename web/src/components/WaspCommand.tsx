import TabItem from "@theme/TabItem";
import Tabs from "@theme/Tabs";
import { type ReactNode } from "react";
import PackageManagerCommand from "./PackageManagerCommand";
import { getCommandFromChildren, PACKAGE_MANAGERS } from "./packageManagers";
import {
  runsWasp,
  toPackageManagerCommands,
  type WaspCli,
} from "./toPackageManagerCommands";

export interface WaspCommandProps {
  /**
   * The command as you'd run it with the global Wasp CLI and npm, e.g.
   * `wasp start`.
   */
  command: string;
}

export type WaspCommandBlockProps = Partial<WaspCommandProps> & {
  /** The command, as an alternative to the `command` prop. */
  children?: ReactNode;
};

/** The tab group that remembers how the reader runs Wasp. */
export const WASP_CLI_TAB_GROUP_ID = "wasp-cli";

const WASP_CLI_TABS: { waspCli: WaspCli; label: string }[] = [
  {
    waspCli: "project",
    label:
      PACKAGE_MANAGERS.length === 1
        ? PACKAGE_MANAGERS[0].label
        : "Package manager",
  },
  { waspCli: "global", label: "Global CLI" },
];

/**
 * Shows a Wasp command the way the reader prefers to run Wasp: through their
 * package manager, or with the global Wasp CLI, e.g.
 * `<WaspCommand command="wasp start" />`. A multi-line command reads better as
 * children:
 *
 * ```mdx
 * <WaspCommand>{`
 * npm install
 * wasp start
 * `}</WaspCommand>
 * ```
 */
export default function WaspCommand({
  command: commandProp,
  children,
}: WaspCommandBlockProps): ReactNode {
  const commandFromChildren = getCommandFromChildren("WaspCommand", children);
  if (commandFromChildren !== undefined && commandProp !== undefined) {
    throw new Error(
      "WaspCommand got its command both as the `command` prop and as children.",
    );
  }
  const command = commandProp ?? commandFromChildren;
  assertRunsWasp("WaspCommand", command);

  return (
    <Tabs groupId={WASP_CLI_TAB_GROUP_ID}>
      {WASP_CLI_TABS.map(({ waspCli, label }) => (
        <TabItem key={waspCli} value={waspCli} label={label}>
          <PackageManagerCommand
            {...toPackageManagerCommands(command, waspCli)}
          />
        </TabItem>
      ))}
    </Tabs>
  );
}

/**
 * MDX props aren't type-checked, so a missing command only shows up here. A
 * command without `wasp` doesn't depend on how the reader runs Wasp, so it
 * doesn't belong in this component.
 */
export function assertRunsWasp(
  componentName: string,
  command: string | undefined,
): asserts command is string {
  if (!command) {
    throw new Error(`${componentName} is missing the \`command\` prop.`);
  }
  if (!runsWasp(command)) {
    throw new Error(
      `${componentName} got a command that doesn't run \`wasp\`: ${JSON.stringify(command)}. Use PackageManagerCommand or a code block instead.`,
    );
  }
}
