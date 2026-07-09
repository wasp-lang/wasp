import { isEqual } from "es-toolkit";
import * as AppSpec from "../../appSpec.js";
import * as WaspSpec from "../publicApi/waspSpec.js";
import { mapRefObject } from "../refObject.js";
import { SpecUserError } from "../specUserError.js";
import {
  AppSpecDeclTypeForWaspSpecElement,
  mapWaspSpecElement,
} from "./specElements.js";

export interface AppMapperContext {
  emitEntityRef(name: string): AppSpec.Ref<"Entity">;

  emitRefObject(refObject: WaspSpec.Reference<unknown>): AppSpec.ExtImport;

  emitRouteRef(name: string): AppSpec.Ref<"Route">;

  emitSpecElementRef<SpecElement extends WaspSpec.SpecElement>(
    specElement: SpecElement,
  ): AppSpec.Ref<AppSpecDeclTypeForWaspSpecElement<SpecElement>>;
}

export function makeAppMapperContext({
  entityNames,
  projectRootDir,
  specElements,
}: {
  entityNames: string[];
  projectRootDir: string;
  specElements: WaspSpec.SpecElement[];
}): {
  ctx: AppMapperContext;
  collectedSpecElementDecls: ReadonlyMap<string, AppSpec.Decl>;
} {
  // Keyed by `declType:declName`: decl names only have to be unique within
  // their decl type (mirroring waspc's AppSpec validation), so decls of
  // different types may share a name.
  const specElementDecls = new Map<string, AppSpec.Decl>();

  const ctx: AppMapperContext = {
    emitEntityRef: makeRefParser("Entity", entityNames),

    emitRefObject: (refObject: unknown) =>
      mapRefObject(refObject, {
        projectRootDir,
      }),

    emitRouteRef: makeRefParser(
      "Route",
      specElements
        .filter((el): el is WaspSpec.Route => el.kind === "route")
        .map((route) => route.name),
    ),

    emitSpecElementRef: <SpecElement extends WaspSpec.SpecElement>(
      specElement: SpecElement,
    ) => {
      const decl = mapWaspSpecElement(specElement, ctx);

      const declKey = `${decl.declType}:${decl.declName}`;
      const oldDecl = specElementDecls.get(declKey);
      if (oldDecl && !isEqual(oldDecl, decl)) {
        throw makeConflictingDeclsError(oldDecl, decl);
      }

      specElementDecls.set(declKey, decl);

      return { declType: decl.declType, name: decl.declName } as AppSpec.Ref<
        AppSpecDeclTypeForWaspSpecElement<SpecElement>
      >;
    },
  };

  return { ctx, collectedSpecElementDecls: specElementDecls };
}

export function makeRefParser<T extends AppSpec.DeclType>(
  declType: T,
  declNames: string[],
): (name: string) => AppSpec.Ref<T> {
  return function parseRef(potentialRef: string): AppSpec.Ref<T> {
    if (!declNames.includes(potentialRef)) {
      throw new SpecUserError(
        `Invalid \`${declType}\` reference: \`${potentialRef}\`\n` +
          `Please make sure that \`${potentialRef}\` is actually defined.`,
      );
    }
    return {
      name: potentialRef,
      declType,
    };
  };
}

function makeConflictingDeclsError(
  existingDecl: AppSpec.Decl,
  incomingDecl: AppSpec.Decl,
): SpecUserError {
  return new SpecUserError(
    `Conflicting configurations for the ${declTypeDisplayNames[existingDecl.declType]} \`${existingDecl.declName}\`:\n` +
      `- Definition A: ${JSON.stringify(existingDecl.declValue)}\n` +
      `- Definition B: ${JSON.stringify(incomingDecl.declValue)}\n\n` +
      `All definitions with the same name must produce the same configuration.\n` +
      "If the duplication was intentional, please use a different name to differentiate them.",
  );
}

const declTypeDisplayNames = {
  App: "app",
  Page: "page",
  Route: "route",
  Query: "query",
  Action: "action",
  Api: "API",
  ApiNamespace: "API namespace",
  Job: "job",
  Crud: "CRUD",
} as const satisfies Record<AppSpec.Decl["declType"], string>;
