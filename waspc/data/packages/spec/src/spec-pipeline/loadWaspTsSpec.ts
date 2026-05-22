import { importWithBundler } from "./importWithBundler.js";
import { typecheck } from "./typecheck.js";

/*
  We do some wrangling because we want to do things in a specific order, to
  match expectations of users:

  1. Read source files
  2. Lower imports
  3. Typecheck
  4. Compile to JS
  5. Run

  However, the TypeScript and jiti APIs don't directly expose the primitives to
  be able to do this. TypeScript will, by default, read and typecheck everything
  before transforming, and jiti will transform and execute on the fly in one go.

  Therefore, we will let jiti transform and run the code. In the `transform`
  function, we will notify the typechecker of any changed source code as we go
  (which will have its imports lowered), so we can overwrite them in the
  TypeScript compiler memory.

  Jiti will immediately run the transformed code. We let it do that. It might
  throw runtime errors, but we capture them and store them for later. We
  typecheck the source code, throw the type errors if any; and only then
  re-throw the errors or re-return the value from the code we've ran.

  The structure then looks like this:

  1. Create the TypeScript project in memory, without any files.
  2. Jiti reads source files
  3. Jiti calls the transform function:
    1. Lower imports
    2. Add the lowered source code to the Typescript project
    3. Pass it to the rest of the jiti pipeline
  4. Jiti automatically runs the user's `wasp.ts` file. We capture the errors or
     result.
  5. We typecheck the files in memory.
  6. Report typechecking result.
  7. Then we return or rethrow the captured result from step 4.

  The only other caveat is that if there are parse errors, we want to report
  those immediately before typechecking, because TypeScript won't have received
  the full picture anyway.
*/
export async function loadWaspTsSpecDefaultExport({
  specPath,
  tsconfigPath,
  projectRootDir,
}: {
  specPath: string;
  tsconfigPath: string;
  projectRootDir: string;
}): Promise<unknown> {
  const specModule = await typecheck(
    { tsconfigPath },
    async ({ addSourceFile }) =>
      await importWithBundler({
        specPath,
        projectRootDir,
        tsconfigPath,
        overwriteTSFile: addSourceFile,
      }),
  );

  return getDefaultExport(specModule);
}

function getDefaultExport(loadedModule: unknown): unknown {
  if (typeof loadedModule !== "object" || loadedModule === null) {
    return undefined;
  }

  return "default" in loadedModule ? loadedModule.default : undefined;
}
