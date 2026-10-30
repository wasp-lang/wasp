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
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path/posix"; // Module paths are always `/`-delimited
import { describe, expect, test } from "vitest";
import { normalizeRefObjectPath } from "../../src/spec/refObjectPath.js";
import { WaspSpecUserError } from "../../src/spec/waspSpecUserError.js";
describe("normalizeRefObjectPath", () => {
    test("maps a top-level spec ref import targeting src", () => {
        const env_1 = { stack: [], error: void 0, hasError: false };
        try {
            const project = __addDisposableResource(env_1, makeTempProject(), false);
            expect(normalizeRefObjectPath({
                importPath: "./src/MainPage",
                importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
                projectRootDir: project.rootDir,
            })).toBe("@src/MainPage");
        }
        catch (e_1) {
            env_1.error = e_1;
            env_1.hasError = true;
        }
        finally {
            __disposeResources(env_1);
        }
    });
    test("maps a nested spec ref import from its importing file", () => {
        const env_2 = { stack: [], error: void 0, hasError: false };
        try {
            const project = __addDisposableResource(env_2, makeTempProject(), false);
            expect(normalizeRefObjectPath({
                importPath: "./MainPage",
                importingFilePath: path.join(project.rootDir, "src", "features", "home.wasp.ts"),
                projectRootDir: project.rootDir,
            })).toBe("@src/features/MainPage");
        }
        catch (e_2) {
            env_2.error = e_2;
            env_2.hasError = true;
        }
        finally {
            __disposeResources(env_2);
        }
    });
    test("maps a nested ref import that climbs to the src root", () => {
        const env_3 = { stack: [], error: void 0, hasError: false };
        try {
            const project = __addDisposableResource(env_3, makeTempProject(), false);
            expect(normalizeRefObjectPath({
                importPath: "../../MainPage",
                importingFilePath: path.join(project.rootDir, "src", "features", "home", "home.wasp.ts"),
                projectRootDir: project.rootDir,
            })).toBe("@src/MainPage");
        }
        catch (e_3) {
            env_3.error = e_3;
            env_3.hasError = true;
        }
        finally {
            __disposeResources(env_3);
        }
    });
    test("normalizes ref import path segments", () => {
        const env_4 = { stack: [], error: void 0, hasError: false };
        try {
            const project = __addDisposableResource(env_4, makeTempProject(), false);
            expect(normalizeRefObjectPath({
                importPath: "./src/features/../MainPage",
                importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
                projectRootDir: project.rootDir,
            })).toBe("@src/MainPage");
        }
        catch (e_4) {
            env_4.error = e_4;
            env_4.hasError = true;
        }
        finally {
            __disposeResources(env_4);
        }
    });
    test("rejects ref imports that resolve outside src", () => {
        const env_5 = { stack: [], error: void 0, hasError: false };
        try {
            const project = __addDisposableResource(env_5, makeTempProject(), false);
            expect(() => normalizeRefObjectPath({
                importPath: "./helpers/helper",
                importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
                projectRootDir: project.rootDir,
            })).toThrow(WaspSpecUserError);
            expect(() => normalizeRefObjectPath({
                importPath: "./helpers/helper",
                importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
                projectRootDir: project.rootDir,
            })).toThrow(/must resolve to a file inside the app src\/ directory/);
        }
        catch (e_5) {
            env_5.error = e_5;
            env_5.hasError = true;
        }
        finally {
            __disposeResources(env_5);
        }
    });
    test("rejects ref imports that resolve to src itself", () => {
        const env_6 = { stack: [], error: void 0, hasError: false };
        try {
            const project = __addDisposableResource(env_6, makeTempProject(), false);
            expect(() => normalizeRefObjectPath({
                importPath: "./src",
                importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
                projectRootDir: project.rootDir,
            })).toThrow(WaspSpecUserError);
            expect(() => normalizeRefObjectPath({
                importPath: "./src",
                importingFilePath: path.join(project.rootDir, "main.wasp.ts"),
                projectRootDir: project.rootDir,
            })).toThrow(/must resolve to a file inside the app src\/ directory/);
        }
        catch (e_6) {
            env_6.error = e_6;
            env_6.hasError = true;
        }
        finally {
            __disposeResources(env_6);
        }
    });
    test("allows ref imports that leave src and resolve back inside", () => {
        const env_7 = { stack: [], error: void 0, hasError: false };
        try {
            const project = __addDisposableResource(env_7, makeTempProject(), false);
            expect(normalizeRefObjectPath({
                importPath: "../../../src/MainPage",
                importingFilePath: path.join(project.rootDir, "src", "features", "home", "home.wasp.ts"),
                projectRootDir: project.rootDir,
            })).toBe("@src/MainPage");
        }
        catch (e_7) {
            env_7.error = e_7;
            env_7.hasError = true;
        }
        finally {
            __disposeResources(env_7);
        }
    });
});
function makeTempProject() {
    const rootDir = fs.mkdtempSync(path.join(os.tmpdir(), "wasp-ref-path-"));
    fs.mkdirSync(path.join(rootDir, "src"));
    return {
        rootDir,
        [Symbol.dispose]: () => fs.rmSync(rootDir, { recursive: true, force: true }),
    };
}
