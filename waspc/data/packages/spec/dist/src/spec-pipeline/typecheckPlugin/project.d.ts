import ts from "typescript";
export declare function typecheckProject({ tsconfigPath, overriddenFiles, }: {
    tsconfigPath: string;
    overriddenFiles: ReadonlyMap<string, string>;
}): {
    diagnostics: readonly ts.Diagnostic[];
    formatDiagnosticsWithColorAndContext: (diagnostics: readonly ts.Diagnostic[]) => string;
};
//# sourceMappingURL=project.d.ts.map