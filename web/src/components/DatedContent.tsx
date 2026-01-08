import Admonition from "@theme/Admonition";
import { useMemo } from "react";

const PROBABLY_OUTDATED_THRESHOLD_MS = 1000 * 60 * 60 * 24 * 90; // 90 days

const FORMATTERS = {
  list: new Intl.ListFormat("en-US", { type: "conjunction" }),
  date: new Intl.DateTimeFormat("en-US", {
    year: "numeric",
    month: "short",
    day: "numeric",
    timeZone: "UTC",
  }),
};

export default function DatedContent({
  frontMatter: { last_update: { date, versions } = {} },
}: {
  frontMatter: {
    last_update?: { date?: string | Date; versions?: Record<string, string> };
  };
}) {
  const updatedString = useMemo(() => {
    const dateString = date && "on " + FORMATTERS.date.format(new Date(date));

    const versionsString =
      versions &&
      "for " +
        FORMATTERS.list.format(
          Object.entries(versions).map(([key, value]) => `${key} ${value}`),
        );

    return [dateString, versionsString].filter(Boolean).join(" ");
  }, [date, versions]);

  const probablyOutdated =
    date && Date.now() - +date > PROBABLY_OUTDATED_THRESHOLD_MS;

  return (
    <Admonition type={probablyOutdated ? "warning" : "note"}>
      {updatedString ? (
        <>
          <strong>Last updated {updatedString}.</strong>
          <br />
        </>
      ) : undefined}
      This guide depends on external libraries or services, so it may become
      outdated over time. We do our best to keep it up to date, but make sure to
      check their documentation for any changes.
    </Admonition>
  );
}
