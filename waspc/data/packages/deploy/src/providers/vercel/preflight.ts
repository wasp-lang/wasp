import fs from "fs";
import path from "node:path";

import { WaspProjectDir } from "../../common/brandedTypes.js";

/**
 * Preflight checks (task-16): best-effort, textual detection of Wasp
 * project features that are known to behave badly on Vercel's serverless
 * platform - pg-boss `job`s (assume a long-lived process, break under
 * scale-to-zero) and `webSocket` usage (beta on Vercel, instance-pinned,
 * bound by the function's maxDuration).
 *
 * Per the project decision ("warn-not-block"), this module only ever warns:
 * it never throws and never affects the deploy's exit code. It is a
 * heuristic, not a full Wasp compiler front-end - it textually scans
 * `main.wasp` / `main.wasp.ts` (and, one hop out, its local relative
 * imports) for `job(...)` calls / `job <name> { ... }` blocks and
 * `webSocket` usage, mirroring how a human would grep the project.
 */

export interface PreflightFindings {
  jobNames: string[];
  hasWebSocket: boolean;
}

export interface PreflightWarning {
  message: string;
}

const ENTRY_FILE_CANDIDATES = ["main.wasp.ts", "main.wasp"];

// How many project files (entry file + local imports) we're willing to
// read while looking for job/webSocket declarations. This is a heuristic
// scan, not a module resolver, so we bound it to avoid runaway traversal.
const MAX_FILES_SCANNED = 50;

// Candidate suffixes to try when resolving a relative import specifier
// (e.g. "./src/features/jobs/jobs.wasp") to a file on disk. Order matters:
// an exact match wins, otherwise fall back to adding an extension.
const RESOLVE_SUFFIXES = ["", ".ts", ".wasp.ts", "/index.ts"];

// New (0.25+/main) TS spec API: `job(someJobIdentifier, { ... })`.
const JOB_CALL_RE = /\bjob\s*\(\s*([A-Za-z_$][\w$]*)/g;
// Legacy `.wasp` DSL: `job someJobName { ... }`.
const JOB_DECL_RE = /(?:^|\n)[ \t]*job[ \t]+([A-Za-z_$][\w$]*)[ \t]*\{/g;
// Matches the `webSocket` key/shorthand on the app config, in both the TS
// object-literal form (`webSocket: {...}` / `webSocket,`) and the legacy
// `.wasp` DSL field, without false-positiving on unrelated identifiers
// like `webSocketServer`.
const WEBSOCKET_RE = /\bwebSocket\b\s*[:,}]/;
const LOCAL_IMPORT_RE = /from\s+["'](\.[^"']+)["']/g;

export function findWaspEntryFile(
  waspProjectDir: WaspProjectDir,
): string | undefined {
  for (const candidate of ENTRY_FILE_CANDIDATES) {
    const candidatePath = path.join(waspProjectDir, candidate);
    if (fs.existsSync(candidatePath) && fs.statSync(candidatePath).isFile()) {
      return candidatePath;
    }
  }
  return undefined;
}

export function scanWaspProjectForPreflightFindings(
  waspProjectDir: WaspProjectDir,
): PreflightFindings {
  const entryFile = findWaspEntryFile(waspProjectDir);
  if (entryFile === undefined) {
    return { jobNames: [], hasWebSocket: false };
  }

  const jobNames = new Set<string>();
  let hasWebSocket = false;

  const visited = new Set<string>();
  const queue: string[] = [entryFile];

  while (queue.length > 0 && visited.size < MAX_FILES_SCANNED) {
    const filePath = queue.shift();
    if (filePath === undefined || visited.has(filePath)) {
      continue;
    }
    visited.add(filePath);

    const content = tryReadFile(filePath);
    if (content === undefined) {
      continue;
    }

    for (const match of content.matchAll(JOB_CALL_RE)) {
      jobNames.add(match[1]);
    }
    for (const match of content.matchAll(JOB_DECL_RE)) {
      jobNames.add(match[1]);
    }
    if (WEBSOCKET_RE.test(content)) {
      hasWebSocket = true;
    }

    const fileDir = path.dirname(filePath);
    for (const match of content.matchAll(LOCAL_IMPORT_RE)) {
      const resolved = resolveLocalImport(fileDir, match[1]);
      if (resolved !== undefined && !visited.has(resolved)) {
        queue.push(resolved);
      }
    }
  }

  return { jobNames: Array.from(jobNames), hasWebSocket };
}

function tryReadFile(filePath: string): string | undefined {
  try {
    return fs.readFileSync(filePath, "utf-8");
  } catch {
    return undefined;
  }
}

function resolveLocalImport(
  fromDir: string,
  specifier: string,
): string | undefined {
  const base = path.join(fromDir, specifier);
  for (const suffix of RESOLVE_SUFFIXES) {
    const candidate = base + suffix;
    if (fs.existsSync(candidate) && fs.statSync(candidate).isFile()) {
      return candidate;
    }
  }
  return undefined;
}

export function buildPreflightWarnings(
  findings: PreflightFindings,
): PreflightWarning[] {
  const warnings: PreflightWarning[] = [];

  if (findings.jobNames.length > 0) {
    const jobList = findings.jobNames.join(", ");
    warnings.push({
      message: [
        `WARN: This Wasp project declares the following pg-boss job(s): ${jobList}.`,
        "pg-boss jobs assume a long-lived process; they can miss scheduled runs or fail",
        "outright on a scale-to-zero platform like Vercel's serverless functions.",
        "Consider using Vercel Cron (https://vercel.com/docs/cron-jobs) to trigger this",
        "work on a schedule instead.",
      ].join(" "),
    });
  }

  if (findings.hasWebSocket) {
    warnings.push({
      message: [
        "WARN: This Wasp project uses WebSockets.",
        "WebSocket support on Vercel is currently in beta: connections are",
        "instance-pinned (they will not survive a scale-to-zero or a redeploy) and are",
        "subject to the function's maxDuration limit.",
        "See https://vercel.com/docs/functions/functions-api-reference#websockets for details.",
      ].join(" "),
    });
  }

  return warnings;
}

/**
 * Runs the preflight checks for the given Wasp project and prints any
 * warnings to stderr via console.warn. Never throws - per the project's
 * "warn-not-block" decision, this must never affect the deploy's exit
 * code or interrupt the calling command.
 */
export function runVercelPreflightChecks(
  waspProjectDir: WaspProjectDir,
): PreflightWarning[] {
  try {
    const findings = scanWaspProjectForPreflightFindings(waspProjectDir);
    const warnings = buildPreflightWarnings(findings);
    for (const warning of warnings) {
      console.warn(warning.message);
    }
    return warnings;
  } catch {
    // Warn-not-block: any unexpected failure in this best-effort scan must
    // never interrupt the deploy.
    return [];
  }
}
