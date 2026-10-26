// Copies the server bundle and only the files it needs at runtime into
// `production/`, so the final Docker image doesn't need the full `node_modules`.
//
// We trace:
// - `bundle/server.js`: the bundled server. Rollup leaves the packages it
//   imports out of the bundle, so following those imports finds the files the
//   server needs at runtime.
// - `dotenv/config`: preloaded with `node -r dotenv/config` in the `start` script.
// - The Prisma CLI, but only when `start-production` runs migrations.
// - The packages the user lists in `app.server.traceDeps`.
import type { TracedLayout } from "nf3";
import { traceNodeModules } from "nf3";
import { FullTracePackages } from "nf3/db";
import {
  cpSync,
  existsSync,
  globSync,
  mkdirSync,
  readFileSync,
  realpathSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";

type ServerPackageJson = {
  name: string;
  scripts: Record<string, string>;
};

type TraceSelection = {
  /** Packages to trace even when nothing imports them. */
  traceInclude: string[];
  /** Packages to copy in full. */
  fullTraceInclude: string[];
};

type PrismaCliEnginesOptions = {
  layout: TracedLayout;
  outDir: string;
  requireFromServer: NodeJS.Require;
};

type ProductionPackageJsonOptions = {
  layout: TracedLayout;
  serverDir: string;
  outDir: string;
  serverPackageJson: ServerPackageJson;
  /** The Prisma CLI's entry file, when the image runs migrations. */
  prismaCliEntry: string | undefined;
};

await traceProductionDeps([]);

/**
 * @param userTraceDeps The packages the user listed in `app.server.traceDeps`.
 */
async function traceProductionDeps(userTraceDeps: string[]): Promise<void> {
  const serverDir = import.meta.dirname;
  const outDir = path.join(serverDir, "production");
  const requireFromServer = createRequire(path.join(serverDir, "package.json"));
  const serverPackageJson: ServerPackageJson = JSON.parse(
    readFileSync(path.join(serverDir, "package.json"), "utf8"),
  );
  const runsMigrations =
    serverPackageJson.scripts["start-production"].includes("db-migrate-prod");
  const prismaCliEntry = runsMigrations
    ? requireFromServer.resolve("prisma/build/index.js")
    : undefined;
  const { traceInclude, fullTraceInclude } = resolveTraceDeps(userTraceDeps);

  rmSync(outDir, { recursive: true, force: true });
  mkdirSync(outDir, { recursive: true });

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
      nft: {
        ignore: (filePath: string) =>
          isTypescriptCompiler(filePath, traceInclude),
      },
    },
  );
  if (prismaCliEntry) {
    copyPrismaCliEngines({ layout, outDir, requireFromServer });
  }
  cpSync(path.join(serverDir, "bundle"), path.join(outDir, "bundle"), {
    recursive: true,
  });
  writeProductionPackageJson({
    layout,
    serverDir,
    outDir,
    serverPackageJson,
    prismaCliEntry,
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
    traceInclude: traceInclude.filter(
      (packageName) => !excluded.has(packageName),
    ),
    fullTraceInclude,
  };
}

/**
 * The Prisma CLI has an optional `require("typescript")` that it never reaches
 * when running migrations, but the tracer follows it and would copy the whole
 * compiler. We skip it unless the user listed it in `app.server.traceDeps`.
 */
function isTypescriptCompiler(
  filePath: string,
  traceInclude: string[],
): boolean {
  return (
    filePath.includes("node_modules/typescript/") &&
    !traceInclude.includes("typescript")
  );
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
 * @see https://github.com/unjs/nf3#readme
 */
function copyPrismaCliEngines({
  layout,
  outDir,
  requireFromServer,
}: PrismaCliEnginesOptions): void {
  const prismaPkgDir = realpathSync(
    path.dirname(requireFromServer.resolve("prisma/package.json")),
  );
  const enginesPkgDir = realpathSync(
    path.dirname(
      createRequire(path.join(prismaPkgDir, "package.json")).resolve(
        "@prisma/engines/package.json",
      ),
    ),
  );
  const enginesOutDir = path.join(
    outDir,
    getHoistedOutPath(layout, "@prisma/engines") ??
      "node_modules/@prisma/engines",
  );
  const clientOutDir = path.join(outDir, "node_modules/.prisma/client");

  mkdirSync(enginesOutDir, { recursive: true });
  for (const engine of globSync(["schema-engine-*", "libquery_engine-*"], {
    cwd: enginesPkgDir,
  })) {
    const engineInClient = path.join(clientOutDir, engine);
    const engineOutPath = path.join(enginesOutDir, engine);
    if (existsSync(engineInClient)) {
      symlinkSync(path.relative(enginesOutDir, engineInClient), engineOutPath);
    } else {
      cpSync(path.join(enginesPkgDir, engine), engineOutPath);
    }
  }
}

/**
 * `npm run` puts `node_modules/.bin` on the PATH, but the traced output has
 * no `.bin` folder, so we call the Prisma CLI through its entry file instead.
 */
function writeProductionPackageJson({
  layout,
  serverDir,
  outDir,
  serverPackageJson,
  prismaCliEntry,
}: ProductionPackageJsonOptions): void {
  const { scripts } = serverPackageJson;
  const productionScripts: Record<string, string> = {
    start: scripts.start,
    "start-production": scripts["start-production"],
  };
  if (prismaCliEntry) {
    const prismaCliInOutDir =
      layout.files[prismaCliEntry] ?? path.relative(serverDir, prismaCliEntry);
    productionScripts["db-migrate-prod"] = scripts["db-migrate-prod"].replace(
      /^prisma /,
      `node ${prismaCliInOutDir} `,
    );
  }

  writeFileSync(
    path.join(outDir, "package.json"),
    JSON.stringify(
      {
        name: serverPackageJson.name,
        private: true,
        type: "module",
        scripts: productionScripts,
      },
      null,
      2,
    ),
  );
}

/** Returns the package's path in `outDir`, if the tracer copied it. */
function getHoistedOutPath(
  layout: TracedLayout,
  packageName: string,
): string | undefined {
  const versions = layout.packages[packageName]?.versions ?? {};
  return Object.values(versions).find((version) => version.hoisted)?.outPath;
}
