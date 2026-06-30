import { useDoc } from "@docusaurus/plugin-content-docs/client";
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

interface FrontMatter {
  last_checked_with_versions?: LastCheckedWithVersions;
}

/**
 * Renders a note listing the versions a guide was last checked with.
 *
 * The `remark-last-checked-with-versions` plugin injects this
 * component just below the title of any guide that declares
 * `last_checked_with_versions` value.
 */
export default function LastCheckedWithVersionsNotice() {
  const { frontMatter } = useDoc();
  const lastCheckedWithVersions = (frontMatter as FrontMatter)
    .last_checked_with_versions;

  const lastCheckedWithString = useMemo(
    () =>
      lastCheckedWithVersions
        ? listFormatter.format(
            Object.entries(lastCheckedWithVersions).map(([key, value]) =>
              formatVersion(key, value),
            ),
          )
        : "",
    [lastCheckedWithVersions],
  );

  if (!lastCheckedWithVersions) {
    return null;
  }

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
