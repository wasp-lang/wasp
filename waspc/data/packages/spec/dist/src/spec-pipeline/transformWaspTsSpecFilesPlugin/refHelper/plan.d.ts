import type { ESTree as t } from "rolldown/utils";
export type Plan = {
    /**
     * The local names `ref` was imported under (e.g. `["ref"]` or `["appRef"]`),
     * one per `ref` specifier we removed. Each gets aliased back to the generated
     * helper so the rest of the file keeps working.
     */
    refHelperLocalNames: string[];
    /** Source ranges to delete */
    removals: SourceRange[];
    safeMakeRefHelperName: string;
};
type SourceRange = {
    start: number;
    end: number;
};
export declare function planTransformRefHelper(ast: t.Program): Plan | null;
export {};
//# sourceMappingURL=plan.d.ts.map