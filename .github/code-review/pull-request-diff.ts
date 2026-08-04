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

  const unquotedPath = rawPath.startsWith('"')
    ? parseQuotedPath(rawPath)
    : rawPath;
  if (!unquotedPath.startsWith("b/")) {
    throw new Error(`Unexpected path in pull request diff: ${rawPath}`);
  }

  return unquotedPath.slice(2);
}

function parseQuotedPath(rawPath: string): string {
  if (!rawPath.endsWith('"')) {
    throw new Error(`Unsupported quoted path in pull request diff: ${rawPath}`);
  }

  const bytes: number[] = [];
  const escapeSequences: Record<string, number> = {
    '"': 0x22,
    "\\": 0x5c,
    a: 0x07,
    b: 0x08,
    f: 0x0c,
    n: 0x0a,
    r: 0x0d,
    t: 0x09,
    v: 0x0b,
  };

  for (let index = 1; index < rawPath.length - 1; index += 1) {
    const character = rawPath[index];
    if (character !== "\\") {
      bytes.push(character.charCodeAt(0));
      continue;
    }

    const escaped = rawPath[++index];
    const octal = rawPath.slice(index, index + 3);
    if (/^[0-7]{3}$/.test(octal)) {
      bytes.push(Number.parseInt(octal, 8));
      index += 2;
    } else if (escaped in escapeSequences) {
      bytes.push(escapeSequences[escaped]);
    } else {
      throw new Error(
        `Unsupported quoted path in pull request diff: ${rawPath}`,
      );
    }
  }

  return new TextDecoder("utf-8", { fatal: true }).decode(
    Uint8Array.from(bytes),
  );
}
