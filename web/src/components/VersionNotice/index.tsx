import { dateFormatter, listFormatter } from "@site/src/lib/formatters";
import Admonition from "@theme/Admonition";
import { useMemo } from "react";
import styles from "./styles.module.css";

// If a string looks like a number or date, YAML will parse it.
// Dates are useful for unversioned dependencies, like Shadcn.
type Version = string | number | Date;

export interface CheckedVersions {
  [name: string]: Version;
}

export default function VersionNotice({
  checkedVersions,
}: {
  checkedVersions: CheckedVersions;
}) {
  const checkedWithString = useMemo(
    () =>
      listFormatter.format(
        Object.entries(checkedVersions).map(([key, value]) =>
          formatVersion(key, value),
        ),
      ),
    [checkedVersions],
  );

  return (
    <Admonition type="note">
      <p className={styles.lastCheckedLine}>
        Last checked with {checkedWithString}.
      </p>
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
