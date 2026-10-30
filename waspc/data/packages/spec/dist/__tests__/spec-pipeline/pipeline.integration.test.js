var __addDisposableResource = (this && this.__addDisposableResource) || function (env, value, async) {
    if (value !== null && value !== void 0) {
        if (typeof value !== "object" && typeof value !== "function") throw new TypeError("Object expected.");
        var dispose, inner;
        if (async) {
            if (!Symbol.asyncDispose) throw new TypeError("Symbol.asyncDispose is not defined.");
            dispose = value[Symbol.asyncDispose];
        }
        if (dispose === void 0) {
            if (!Symbol.dispose) throw new TypeError("Symbol.dispose is not defined.");
            dispose = value[Symbol.dispose];
            if (async) inner = dispose;
        }
        if (typeof dispose !== "function") throw new TypeError("Object not disposable.");
        if (inner) dispose = function() { try { inner.call(this); } catch (e) { return Promise.reject(e); } };
        env.stack.push({ value: value, dispose: dispose, async: async });
    }
    else if (async) {
        env.stack.push({ async: true });
    }
    return value;
};
var __disposeResources = (this && this.__disposeResources) || (function (SuppressedError) {
    return function (env) {
        function fail(e) {
            env.error = env.hasError ? new SuppressedError(e, env.error, "An error was suppressed during disposal.") : e;
            env.hasError = true;
        }
        var r, s = 0;
        function next() {
            while (r = env.stack.pop()) {
                try {
                    if (!r.async && s === 1) return s = 0, env.stack.push(r), Promise.resolve().then(next);
                    if (r.dispose) {
                        var result = r.dispose.call(r.value);
                        if (r.async) return s |= 2, Promise.resolve(result).then(next, function(e) { fail(e); return next(); });
                    }
                    else s |= 1;
                }
                catch (e) {
                    fail(e);
                }
            }
            if (s === 1) return env.hasError ? Promise.reject(env.error) : Promise.resolve();
            if (env.hasError) throw env.error;
        }
        return next();
    };
})(typeof SuppressedError === "function" ? SuppressedError : function (error, suppressed, message) {
    var e = new Error(message);
    return e.name = "SuppressedError", e.error = error, e.suppressed = suppressed, e;
});
import * as cp from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { describe, expect, test } from "vitest";
const SPEC_PACKAGE_DIR = path.resolve(import.meta.dirname, "..", "..");
describe("Wasp TS spec pipeline", () => {
    test("analyzes split specs with lowered ref imports", () => {
        const env_1 = { stack: [], error: void 0, hasError: false };
        try {
            const project = __addDisposableResource(env_1, makeTempProject("wasp-spec-pipeline-"), false);
            project.writeProjectFile("src/MainPage.ts", `export default function MainPage() { return null; }\n`);
            project.writeProjectFile("src/adminOperations.ts", [
                `throw new Error("ref import was executed");`,
                `export async function archive() { return null; }`,
                ``,
            ].join("\n"));
            project.writeProjectFile("src/features/home.wasp.ts", [
                `import { page } from "@wasp/spec";`,
                `import MainPage from "../MainPage" with { type: "ref" };`,
                ``,
                `export const homePage = page(MainPage);`,
            ].join("\n"));
            project.writeProjectFile("src/features/tasks.wasp.ts", [
                `import { action } from "@wasp/spec";`,
                `import { archive } from "../adminOperations" with { type: "ref" };`,
                ``,
                `export const splitTitle = "Split Demo";`,
                `export const archiveAction = action(archive, { entities: [] });`,
            ].join("\n"));
            project.writeProjectFile("src/features/faq.wasp.ts", [
                `import { page } from "@wasp/spec";`,
                `import { splitTitle } from "./tasks.wasp";`,
                `import FaqPage from "./faq/FaqPage" with { type: "ref" };`,
                ``,
                `export const faqPage = page(FaqPage);`,
            ].join("\n"));
            project.writeProjectFile("src/features/faq/FaqPage.ts", [`export defautl function FaqPage() { return null; }`].join("\n"));
            const result = project.analyzeSpec([
                `import { app } from "@wasp/spec";`,
                `import { homePage } from "./src/features/home.wasp";`,
                `import { archiveAction, splitTitle } from "./src/features/tasks.wasp";`,
                `import { faqPage } from "./src/features/faq.wasp";`,
                ``,
                `export default app({`,
                `  name: "demo",`,
                `  title: splitTitle,`,
                `  wasp: { version: "^0.16.0" },`,
                `  spec: [homePage, archiveAction, faqPage],`,
                `});`,
            ].join("\n"));
            expect(result).toMatchSnapshot();
        }
        catch (e_1) {
            env_1.error = e_1;
            env_1.hasError = true;
        }
        finally {
            __disposeResources(env_1);
        }
    });
    test("surfaces type errors in the spec as a WaspSpecUserError with formatted diagnostics", () => {
        const env_2 = { stack: [], error: void 0, hasError: false };
        try {
            const project = __addDisposableResource(env_2, makeTempProject("wasp-spec-pipeline-type-error-"), false);
            const result = project.analyzeSpec([
                `import { app } from "@wasp/spec";`,
                ``,
                `export const oops: string = 123;`,
                ``,
                `export default app({`,
                `  name: "demo",`,
                `  title: "Demo",`,
                `  wasp: { version: "^0.16.0" },`,
                `  spec: [],`,
                `});`,
            ].join("\n"));
            expect(result).toEqual({
                status: "error",
                error: expect.stringContaining("Type 'number' is not assignable to type 'string'"),
            });
        }
        catch (e_2) {
            env_2.error = e_2;
            env_2.hasError = true;
        }
        finally {
            __disposeResources(env_2);
        }
    });
    test("surfaces a WaspSpecUserError thrown by the user's spec as a clean error", () => {
        const env_3 = { stack: [], error: void 0, hasError: false };
        try {
            const project = __addDisposableResource(env_3, makeTempProject("wasp-spec-pipeline-user-error-"), false);
            const result = project.analyzeSpec([
                `import { app, WaspSpecUserError } from "@wasp/spec";`,
                ``,
                `function requireEnv(name: string): string {`,
                `  throw new WaspSpecUserError(\`Missing environment variable '\${name}'.\`);`,
                `}`,
                ``,
                `export default app({`,
                `  name: "demo",`,
                `  title: requireEnv("MY_SECRET"),`,
                `  wasp: { version: "^0.16.0" },`,
                `  spec: [],`,
                `});`,
            ].join("\n"));
            expect(result).toEqual({
                status: "error",
                error: "Missing environment variable 'MY_SECRET'.",
            });
        }
        catch (e_3) {
            env_3.error = e_3;
            env_3.hasError = true;
        }
        finally {
            __disposeResources(env_3);
        }
    });
    test("rejects namespace ref imports with a WaspSpecUserError", () => {
        const env_4 = { stack: [], error: void 0, hasError: false };
        try {
            const project = __addDisposableResource(env_4, makeTempProject("wasp-spec-pipeline-namespace-"), false);
            project.writeProjectFile("src/operations.ts", `export async function archive() { return null; }\n`);
            const result = project.analyzeSpec([
                `import { app } from "@wasp/spec";`,
                `import * as ops from "./src/operations" with { type: "ref" };`,
                ``,
                `export default app({`,
                `  name: "demo",`,
                `  title: "Demo",`,
                `  wasp: { version: "^0.16.0" },`,
                `  spec: [],`,
                `});`,
            ].join("\n"));
            expect(result).toEqual({
                status: "error",
                error: expect.stringContaining("Namespace imports are not supported for reference imports"),
            });
        }
        catch (e_4) {
            env_4.error = e_4;
            env_4.hasError = true;
        }
        finally {
            __disposeResources(env_4);
        }
    });
});
function makeTempProject(prefix) {
    const projectRootDir = fs.mkdtempSync(path.join(os.tmpdir(), prefix));
    return scaffoldProject({
        projectRootDir,
        dispose: () => fs.rmSync(projectRootDir, { recursive: true, force: true }),
    });
}
function scaffoldProject({ projectRootDir, dispose, }) {
    const tsconfigPath = path.join(projectRootDir, "tsconfig.json");
    fs.writeFileSync(path.join(projectRootDir, "package.json"), JSON.stringify({
        type: "module",
        dependencies: { "@wasp/spec": "file:" + SPEC_PACKAGE_DIR },
    }));
    fs.writeFileSync(tsconfigPath, JSON.stringify({
        compilerOptions: {
            target: "ES2022",
            module: "ESNext",
            moduleResolution: "bundler",
            jsx: "preserve",
            strict: true,
            allowJs: true,
            noEmit: true,
        },
        include: ["main.wasp.ts", "**/*.wasp.ts"],
    }));
    return {
        [Symbol.dispose]: dispose,
        writeProjectFile: (relativeFilePath, sourceText) => {
            writeProjectFile(projectRootDir, relativeFilePath, sourceText);
        },
        analyzeSpec: (sourceText) => {
            writeProjectFile(projectRootDir, "main.wasp.ts", sourceText);
            cp.execSync("npm i", { cwd: projectRootDir, stdio: "inherit" });
            cp.execSync("npx @wasp/spec analyze main.wasp.ts tsconfig.json . result.json '[]'", { cwd: projectRootDir, stdio: "inherit" });
            return JSON.parse(fs.readFileSync(path.join(projectRootDir, "result.json"), "utf8"));
        },
    };
}
function writeProjectFile(projectRootDir, relativeFilePath, sourceText) {
    const filePath = path.join(projectRootDir, relativeFilePath);
    writeFile(filePath, sourceText);
}
function writeFile(filePath, sourceText) {
    fs.mkdirSync(path.dirname(filePath), { recursive: true });
    fs.writeFileSync(filePath, sourceText, "utf8");
}
