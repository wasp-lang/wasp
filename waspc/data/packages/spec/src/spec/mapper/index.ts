import * as AppSpec from "../../appSpec.js";
import * as WaspSpec from "../publicApi/waspSpec.js";
import { mapApp } from "./app.js";
import { makeAppMapperContext } from "./context.js";

export function convertWaspSpecToAppSpec(
  app: WaspSpec.App,
  {
    projectRootDir,
    entityNames,
  }: {
    projectRootDir: string;
    entityNames: string[];
  },
): AppSpec.Decl[] {
  const { ctx, collectedSpecElementDecls } = makeAppMapperContext({
    entityNames,
    projectRootDir,
  });

  const specElements = flattenSpecElements(app.spec);
  for (const specElement of specElements) {
    ctx.emitSpecElementRef(specElement);
  }

  const appDecl = mapApp(app, ctx);

  return [appDecl, ...collectedSpecElementDecls.values()];
}

function flattenSpecElements(spec: WaspSpec.Spec): WaspSpec.SpecElement[] {
  // We assert the `[spec]` as a `SpecElement[]` to avoid
  // infinite recursion of the `WaspSpec.Spec` type.
  return ([spec] as WaspSpec.SpecElement[]).flat(Infinity);
}
