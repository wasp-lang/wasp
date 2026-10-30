import { type ESTree as t } from "rolldown/utils";
export declare const PUBLIC_REF_HELPER_IMPORT_SOURCE = "@wasp/spec";
export declare const PUBLIC_REF_HELPER_IMPORT_NAME = "ref";
export declare const INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE = "@wasp/spec/internal";
export declare const INTERNAL_MAKE_REF_HELPER_IMPORT_NAME = "_waspMakeRef";
export declare function getStringValue(node: t.IdentifierName | t.StringLiteral): string;
export declare function getTopLevelBindings(ast: t.Program): Set<string>;
export declare function makeSafeName(desiredName: string, definedBindings: ReadonlySet<string>): string;
export declare function buildImportStatement(imports: readonly [importName: string, localName?: string][], source: string): string;
//# sourceMappingURL=util.d.ts.map