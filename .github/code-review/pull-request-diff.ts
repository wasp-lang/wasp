import type { NewFinding } from "./schema.ts";

type DiffHunk = {
  visibleLines: Set<number>;
  addedLines: Set<number>;
};

export type PullRequestDiff = Map<string, DiffHunk[]>;

export function parsePullRequestDiff(diff: string): PullRequestDiff {
  const files: PullRequestDiff = new Map();
  let currentPath: string | null = null;
  let currentHunk: DiffHunk | null = null;
  let expectsNewPath = false;
  let newLine = 0;

  for (const line of diff.split("\n")) {
    const hunkHeader = line.match(/^@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@/);
    if (hunkHeader && currentPath) {
      newLine = Number(hunkHeader[1]);
      currentHunk = { visibleLines: new Set(), addedLines: new Set() };
      const hunks = files.get(currentPath) ?? [];
      hunks.push(currentHunk);
      files.set(currentPath, hunks);
      continue;
    }

    if (currentHunk) {
      if (line.startsWith("+")) {
        currentHunk.visibleLines.add(newLine);
        currentHunk.addedLines.add(newLine);
        newLine += 1;
        continue;
      }
      if (line.startsWith(" ")) {
        currentHunk.visibleLines.add(newLine);
        newLine += 1;
        continue;
      }
      if (line.startsWith("-") || line === "\\ No newline at end of file") {
        continue;
      }
      currentHunk = null;
    }

    if (line.startsWith("--- ")) {
      expectsNewPath = true;
    } else if (expectsNewPath && line.startsWith("+++ ")) {
      currentPath = parseNewPath(line.slice(4));
      expectsNewPath = false;
    } else if (line.startsWith("diff --git ")) {
      currentPath = null;
      expectsNewPath = false;
    }
  }

  return files;
}

export function isFindingOnChangedLines(
  finding: NewFinding,
  diff: PullRequestDiff,
): boolean {
  if (finding.endLine < finding.startLine) return false;

  const hunks = diff.get(finding.path) ?? [];

  return hunks.some((hunk) => {
    const rangeLength = finding.endLine - finding.startLine + 1;
    if (
      rangeLength > hunk.visibleLines.size ||
      !hunk.addedLines.has(finding.endLine)
    ) {
      return false;
    }

    for (let line = finding.startLine; line <= finding.endLine; line += 1) {
      if (!hunk.visibleLines.has(line)) return false;
    }
    return true;
  });
}

function parseNewPath(rawPath: string): string | null {
  if (rawPath === "/dev/null") return null;
  if (!rawPath.startsWith("b/")) {
    throw new Error(`Unexpected path in pull request diff: ${rawPath}`);
  }

  return rawPath.slice(2);
}
