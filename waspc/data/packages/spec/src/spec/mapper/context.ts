import { isEqual } from "es-toolkit";
import * as AppSpec from "../../appSpec.js";
import * as WaspSpec from "../publicApi/waspSpec.js";
import { mapRefObject } from "../refObject.js";
import { SpecUserError } from "../specUserError.js";
import {
  AppSpecDeclTypeForWaspSpecElement,
  declToRef,
  mapSpecElement,
} from "./specElements.js";

export interface AppMapperContext {
  parseRefObject(refObject: WaspSpec.Reference<unknown>): AppSpec.ExtImport;

  resolveEntityRef(name: string): AppSpec.Ref<"Entity">;

  collectSpecElement<SpecElement extends WaspSpec.SpecElement>(
    specElement: SpecElement,
  ): AppSpec.Ref<AppSpecDeclTypeForWaspSpecElement<SpecElement>>;
}

export function makeAppMapperContext({
  entityNames,
  projectRootDir,
}: {
  entityNames: string[];
  projectRootDir: string;
}): {
  ctx: AppMapperContext;
  collectedDeclsByKey: ReadonlyMap<string, AppSpec.Decl>;
} {
  const collectedDeclsByKey = new Map<string, AppSpec.Decl>();

  const ctx: AppMapperContext = {
    resolveEntityRef: makeRefParser("Entity", entityNames),

    parseRefObject: (refObject) =>
      mapRefObject(refObject, {
        projectRootDir,
      }),

    collectSpecElement: <SpecElement extends WaspSpec.SpecElement>(
      specElement: SpecElement,
    ) => {
      const newDecl = mapSpecElement(specElement, ctx);
      const declKey = makeKeyForDecl(newDecl);

      const existingDecl = collectedDeclsByKey.get(declKey);
      if (existingDecl && !isEqual(existingDecl, newDecl)) {
        throw makeConflictingDeclsError(existingDecl, newDecl);
      }

      collectedDeclsByKey.set(declKey, newDecl);

      return declToRef(newDecl);
    },
  };

  return { ctx, collectedDeclsByKey };
}

function makeKeyForDecl(decl: AppSpec.Decl): string {
  // We're keying by type+name since waspc allows Decls with the same name
  // if they are different types.
  return `${decl.declType}:${decl.declName}`;
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
    `Conflicting configurations for the ${declTypeDisplayNames[existingDecl.declType]} \`${existingDecl.declName}\`:\n\n` +
      `\`${existingDecl.declName}\` (A):\n${showDecl(existingDecl)}\n\n` +
      `\`${incomingDecl.declName}\` (B):\n${showDecl(incomingDecl)}\n\n` +
      `All definitions with the same name must produce the same configuration.\n` +
      "If the duplication was intentional, please use a different name to differentiate them.",
  );
}

const declTypeDisplayNames: Record<AppSpec.Decl["declType"], string> = {
  App: "app",
  Page: "page",
  Route: "route",
  Query: "query",
  Action: "action",
  Api: "API",
  ApiNamespace: "API namespace",
  Job: "job",
  Crud: "CRUD",
};

function showDecl(decl: AppSpec.Decl) {
  return JSON.stringify(decl.declValue, null, 2);
}
