import type * as AppSpec from "../appSpec.js";
/**
 * Converts a relative ref object path from the user's `.wasp.ts` file into
 * AppSpec's absolute `@src/...` path format.
 *
 * This keeps user-authored paths source-relative while giving the Haskell side
 * the same project-rooted paths it already understands.
 *
 * For example, `./src/MainPage` from `/app/main.wasp.ts` becomes
 * `@src/MainPage`, while `./LoginPage` from `/app/src/auth/auth.wasp.ts`
 * becomes `@src/auth/LoginPage`.
 */
export declare function normalizeRefObjectPath({ importPath, importingFilePath, projectRootDir, }: {
    importPath: string;
    importingFilePath: string;
    projectRootDir: string;
}): AppSpec.ExtImport["path"];
//# sourceMappingURL=refObjectPath.d.ts.map