import { dateFormatter, listFormatter } from "@site/src/lib/formatters";
import Admonition from "@theme/Admonition";
import { useMemo } from "react";

/**
 * A version can be a simple string (e.g., "v2.3.4"), a number (e.g., 14),
 * or a Date (e.g., 2023-01-15) indicating when the dependency was last checked.
 *
 * The YAML parser in Docusaurus automatically tries to parse any number-like or
 * date-like strings into numbers or Date objects, so we need to account for that.
 *
 * Dates are especially useful for unversioned dependencies, like Shadcn.
 */
export type Version = string | number | Date;

export interface LastCheckedWithVersions {
  [name: string]: Version;
}

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
