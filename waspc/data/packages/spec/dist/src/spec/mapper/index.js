import { mapAppSpec } from "./app.js";
import { makeAppMapperContext } from "./context.js";
export function convertWaspSpecToAppSpec(app, { projectRootDir, entityNames, }) {
    const specElements = flattenSpecElements(app.spec);
    const { ctx, collectedDeclsByKey } = makeAppMapperContext({
        entityNames,
        projectRootDir,
        specElements,
    });
    for (const specElement of specElements) {
        ctx.collectSpecElement(specElement);
    }
    const appDecl = mapAppSpec(app, ctx);
    return [appDecl, ...collectedDeclsByKey.values()];
}
function flattenSpecElements(spec) {
    // We assert the `[spec]` as a `SpecElement[]` to avoid
    // infinite recursion of the `WaspSpec.Spec` type.
    return [spec].flat(Infinity);
}
