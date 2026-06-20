import ts from "typescript";

const fileName = "C:/test/main.wasp.ts";
const source = [
  'import { app } from "@wasp.sh/spec";',
  "export default app({",
  "  head: (",
  "    <>",
  '      <link rel="icon" href="/favicon.ico" />',
  "    </>",
  "  ),",
  "  spec: [],",
  "});",
].join("\n");

// Test 1: default ScriptKind (derived from .ts extension)
const sf1 = ts.createSourceFile(fileName, source, ts.ScriptTarget.Latest, true);
console.log("Default scriptKind:", sf1.scriptKind, "(TS =", ts.ScriptKind.TS, ")");

// Test 2: explicit ScriptKind.TSX
const sf2 = ts.createSourceFile(fileName, source, ts.ScriptTarget.Latest, true, ts.ScriptKind.TSX);
console.log("TSX scriptKind:", sf2.scriptKind, "(TSX =", ts.ScriptKind.TSX, ")");

// Test: create program with host that returns TSX source file
const host = {
  getSourceFile: (fn) => {
    if (fn === fileName) {
      return ts.createSourceFile(fileName, source, ts.ScriptTarget.Latest, true, ts.ScriptKind.TSX);
    }
    return undefined;
  },
  getDefaultLibFileName: () => "lib.d.ts",
  writeFile: () => {},
  getCurrentDirectory: () => ".",
  getCanonicalFileName: (f) => f,
  useCaseSensitiveFileNames: () => false,
  getNewLine: () => "\n",
  fileExists: (f) => f === fileName,
  readFile: (f) => (f === fileName ? source : undefined),
};

const program = ts.createProgram({
  rootNames: [fileName],
  options: {
    jsx: ts.JsxEmit.Preserve,
    strict: true,
    noEmit: true,
    target: ts.ScriptTarget.ES2022,
    module: ts.ModuleKind.ESNext,
    moduleResolution: ts.ModuleResolutionKind.Bundler,
  },
  host,
});

const diagnostics = ts.getPreEmitDiagnostics(program);
console.log("Number of diagnostics:", diagnostics.length);
for (const d of diagnostics) {
  console.log(" -", ts.flattenDiagnosticMessageText(d.messageText, "\n").slice(0, 100));
}
