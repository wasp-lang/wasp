/*
  Copies the server bundle and only the files it needs at runtime into
  `production/`, so the final Docker image doesn't need the full `node_modules`.

  We trace:
  - `bundle/server.js`: the bundled server. Rollup leaves the packages it
    imports out of the bundle, so following those imports finds the files the
    server needs at runtime.
  - `dotenv/config`: preloaded with `node -r dotenv/config` in the `start` script.
  - The Prisma CLI, but only when `start-production` runs migrations.
  - The packages the user lists in `app.server.traceDeps`.

  TODO: Research whether we can keep the Prisma CLI out of the production image.
*/
import { traceNodeModules, type TracedLayout } from "nf3";
import { FullTracePackages } from "nf3/db";
import {
  cp,
  glob,
  mkdir,
  readFile,
  rm,
  stat,
  symlink,
} from "node:fs/promises";
import { createRequire } from "node:module";
import path from "node:path";

type TraceOptions = {
  /** The packages the user listed in `app.server.traceDeps`. */
  traceDeps: string[];
  /** Whether `start-production` runs the migrations, which needs the Prisma CLI. */
  runsMigrations: boolean;
};

type TraceSelection = {
  /** Packages to trace even when nothing imports them. */
  traceInclude: string[];
  /** Packages to copy in full. */
  fullTraceInclude: string[];
};

const serverDir = import.meta.dirname;
const outDir = path.join(serverDir, "production");
const requireFromServer = createRequire(path.join(serverDir, "package.json"));

await prepareProduction({
  traceDeps: [],
  runsMigrations: true,
});

async function prepareProduction({
  traceDeps,
  runsMigrations,
}: TraceOptions): Promise<void> {
  const prismaCliEntry = runsMigrations
    ? requireFromServer.resolve("prisma/build/index.js")
    : undefined;
  const { traceInclude, fullTraceInclude } = resolveTraceDeps(traceDeps);

  await rm(outDir, { recursive: true, force: true });
  const layout = await traceNodeModules(
    [
      path.join(serverDir, "bundle/server.js"),
      requireFromServer.resolve("dotenv/config"),
      ...(prismaCliEntry ? [prismaCliEntry] : []),
    ],
    {
      rootDir: serverDir,
      outDir,
      chmod: true,
      traceInclude,
      fullTraceInclude,
    },
  );
  if (prismaCliEntry) {
    await copyPrismaCliEngines();
  }
  await linkPackageBins(layout);
  await cp(path.join(serverDir, "bundle"), path.join(outDir, "bundle"), {
    recursive: true,
  });

  console.log(
    `Traced ${Object.keys(layout.packages).length} packages into ${outDir}`,
  );
}

/**
 * Splits `app.server.traceDeps` into nf3 options, the same way Nitro does:
 * `pkg` traces the package, `pkg*` copies all its files, and `!pkg` removes a
 * package from nf3's built-in list of packages to copy in full.
 *
 * TODO: Remove once we migrate to Nitro, which parses `traceDeps` itself.
 *
 * @see https://nitro.build/config#tracedeps
 */
function resolveTraceDeps(traceDeps: string[]): TraceSelection {
  const excluded = new Set<string>();
  const traceInclude: string[] = [];
  const userFullTraceInclude: string[] = [];

  for (const selector of traceDeps) {
    if (selector.startsWith("!")) {
      excluded.add(selector.slice(1));
    } else if (selector.endsWith("*")) {
      const packageName = selector.slice(0, -1);
      traceInclude.push(packageName);
      userFullTraceInclude.push(packageName);
    } else {
      traceInclude.push(selector);
    }
  }

  const fullTraceInclude = [
    ...new Set([...FullTracePackages, ...userFullTraceInclude]),
  ].filter((packageName) => !excluded.has(packageName));

  return {
    // Traced as if imported: only the files they reach get copied.
    traceInclude: traceInclude.filter(
      (packageName) => !excluded.has(packageName),
    ),
    // Copied whole, for packages that load their own files by path.
    fullTraceInclude,
  };
}

/**
 * The tracer misses the Prisma CLI's engine binaries (the schema engine and
 * the CLI's own copy of the query engine), because the CLI finds them by path
 * at runtime instead of importing them. Without them, the CLI downloads them
 * on every container start, or crashes when there's no internet access.
 *
 * The generated Prisma client already brings the same query engine, so we
 * link the CLI's copy to it instead of shipping the binary twice.
 *
 * Wasp pins a single Prisma version, so both packages sit at their usual
 * paths in the traced output.
 *
 * TODO: Remove once we upgrade to Prisma 8, whose CLI no longer uses engine
 * binaries.
 */
async function copyPrismaCliEngines(): Promise<void> {
  const requireFromPrisma = createRequire(
    requireFromServer.resolve("prisma/package.json"),
  );
  const enginesPkgDir = path.dirname(
    requireFromPrisma.resolve("@prisma/engines/package.json"),
  );
  const enginesOutDir = path.join(outDir, "node_modules/@prisma/engines");
  const clientOutDir = path.join(outDir, "node_modules/.prisma/client");

  for await (const engine of glob(["schema-engine-*", "libquery_engine-*"], {
    cwd: enginesPkgDir,
  })) {
    const engineInClient = path.join(clientOutDir, engine);
    const engineOutPath = path.join(enginesOutDir, engine);
    if (await exists(engineInClient)) {
      await symlink(path.relative(enginesOutDir, engineInClient), engineOutPath);
    } else {
      await cp(path.join(enginesPkgDir, engine), engineOutPath);
    }
  }
}

/**
 * `production/` has no `node_modules/.bin`, because `npm install` never ran in
 * `production/`. We link the bins of the traced packages, so production
 * scripts can call them (e.g. `prisma`). We skip bins whose files weren't
 * traced, and keep the first package that claims a name.
 */
async function linkPackageBins(layout: TracedLayout): Promise<void> {
  const binDir = path.join(outDir, "node_modules/.bin");
  await mkdir(binDir, { recursive: true });

  for (const tracedPackage of Object.values(layout.packages)) {
    const { path: packageDir, outPath } =
      tracedPackage.versions[tracedPackage.hoisted];
    for (const [binName, binFile] of Object.entries(
      await readBins(tracedPackage.name, packageDir),
    )) {
      const binOutPath = path.join(outDir, outPath, binFile);
      const binLinkPath = path.join(binDir, binName);
      if ((await exists(binOutPath)) && !(await exists(binLinkPath))) {
        await symlink(path.relative(binDir, binOutPath), binLinkPath);
      }
    }
  }
}

/**
 * Reads the `bin` field of a package's `package.json`, which is either a
 * single path named after the package or a map of names to paths. Some traced
 * folders (e.g. `.prisma`) have no `package.json`, so they have no bins.
 */
async function readBins(
  packageName: string,
  packageDir: string,
): Promise<Record<string, string>> {
  const packageJsonPath = path.join(packageDir, "package.json");
  if (!(await exists(packageJsonPath))) {
    return {};
  }
  const { bin } = JSON.parse(await readFile(packageJsonPath, "utf8"));
  if (typeof bin === "string") {
    return { [packageName.split("/").pop()!]: bin };
  }
  return bin ?? {};
}

async function exists(filePath: string): Promise<boolean> {
  return stat(filePath).then(
    () => true,
    () => false,
  );
}
