import type { ESTree as t } from "rolldown/utils";
import type { DefaultRefObjectDescriptor, NamedRefObjectDescriptor } from "@wasp/spec";
export type Plan = {
    refImports: RefImport[];
    safeRefHelperName: string;
};
export type RefImportReference = {
    kind: "named";
    refObject: NamedRefObjectDescriptor;
} | {
    kind: "default";
    refObject: DefaultRefObjectDescriptor;
} | {
    kind: "namespace";
    from: string;
    alias: string;
};
export interface RefImport {
    references: RefImportReference[];
    removeImport: {
        start: number;
        end: number;
    };
}
export declare function planTransformImports(ast: t.Program): Plan | null;
//# sourceMappingURL=plan.d.ts.map