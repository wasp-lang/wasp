import { dateFormatter, listFormatter } from "@site/src/lib/formatters";
import Admonition from "@theme/Admonition";
import { useMemo } from "react";

/**
 * A version can be a simple string (e.g., "v2.3.4"), a number (e.g., 14),
 * or a Date (e.g., new Date("2023-01-15")) indicating when the dependency was
 * last checked.
 *
 * Dates are especially useful for unversioned dependencies, like Shadcn.
 */
export type Version = string | number | Date;

export interface LastCheckedWithVersions {
  [name: string]: Version;
}

/**
 * Renders a note listing the versions a guide was last checked with.
 *
 * Place it just below the title of any guide that depends on external libraries
 * or services, passing the versions it was last verified against.
 */
export default function LastCheckedWithVersionsNotice({
  lastCheckedWithVersions,
}: {
  lastCheckedWithVersions: LastCheckedWithVersions;
}) {
  const lastCheckedWithString = useMemo(
    () =>
      listFormatter.format(
        Object.entries(lastCheckedWithVersions).map(([key, value]) =>
          formatVersion(key, value),
        ),
      ),
    [lastCheckedWithVersions],
  );

  return (
    <Admonition type="note">
      <p className="font-bold">Last checked with {lastCheckedWithString}.</p>
      This guide depends on external libraries or services, so it may become
      outdated over time. We do our best to keep it up to date, but make sure to
      check their documentation for any changes.
    </Admonition>
  );
}

function formatVersion(name: string, version: Version): string {
  if (version instanceof Date) {
    return `${name} (as of ${dateFormatter.format(version)})`;
  } else {
    return `${name} ${version}`;
  }
}
