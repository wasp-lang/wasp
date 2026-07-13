import * as AppSpec from "../../appSpec.js";
import * as WaspSpec from "../publicApi/waspSpec.js";
import { mapAppSpec } from "./app.js";
import { makeAppMapperContext } from "./context.js";

export function convertWaspSpecToAppSpec(
  app: WaspSpec.App,
  { entityNames }: { entityNames: string[] },
): AppSpec.Decl[] {
  const specElements = flattenSpecElements(app.spec);

  const { ctx, collectedDeclsByKey } = makeAppMapperContext({
    entityNames,
    specElements,
  });

  for (const specElement of specElements) {
    ctx.collectSpecElement(specElement);
  }

  const appDecl = mapAppSpec(app, ctx);

  return [appDecl, ...collectedDeclsByKey.values()];
}

function flattenSpecElements(spec: WaspSpec.Spec): WaspSpec.SpecElement[] {
  // We assert the `[spec]` as a `SpecElement[]` to avoid
  // infinite recursion of the `WaspSpec.Spec` type.
  return ([spec] as WaspSpec.SpecElement[]).flat(Infinity);
}
