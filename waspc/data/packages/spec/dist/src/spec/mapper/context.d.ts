import * as AppSpec from "../../appSpec.js";
import * as WaspSpec from "../publicApi/waspSpec.js";
import { AppSpecDeclTypeForWaspSpecElement } from "./specElements.js";
export interface AppMapperContext {
    parseRefObject(refObject: WaspSpec.Reference<unknown>): AppSpec.ExtImport;
    resolveEntityRef(name: string): AppSpec.Ref<"Entity">;
    resolveRouteRef(name: string): AppSpec.Ref<"Route">;
    collectSpecElement<SpecElement extends WaspSpec.SpecElement>(specElement: SpecElement): AppSpec.Ref<AppSpecDeclTypeForWaspSpecElement<SpecElement>>;
}
export declare function makeAppMapperContext({ entityNames, projectRootDir, specElements, }: {
    entityNames: string[];
    projectRootDir: string;
    specElements: WaspSpec.SpecElement[];
}): {
    ctx: AppMapperContext;
    collectedDeclsByKey: ReadonlyMap<string, AppSpec.Decl>;
};
export declare function makeRefParser<T extends AppSpec.DeclType>(declType: T, declNames: string[]): (name: string) => AppSpec.Ref<T>;
//# sourceMappingURL=context.d.ts.map