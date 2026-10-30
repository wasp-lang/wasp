import { isEqual } from "es-toolkit";
import { mapRefObject } from "../refObject.js";
import { WaspSpecUserError } from "../waspSpecUserError.js";
import { declToRef, mapSpecElement, } from "./specElements.js";
export function makeAppMapperContext({ entityNames, projectRootDir, specElements, }) {
    const collectedDeclsByKey = new Map();
    const ctx = {
        resolveEntityRef: makeRefParser("Entity", entityNames),
        parseRefObject: (refObject) => mapRefObject(refObject, {
            projectRootDir,
        }),
        resolveRouteRef: makeRefParser("Route", specElements
            .filter((el) => el.kind === "route")
            .map((route) => route.name)),
        collectSpecElement: (specElement) => {
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
function makeKeyForDecl(decl) {
    // We're keying by type+name since waspc allows Decls with the same name
    // if they are different types.
    return `${decl.declType}:${decl.declValue.name}`;
}
export function makeRefParser(declType, declNames) {
    return function parseRef(potentialRef) {
        if (!declNames.includes(potentialRef)) {
            throw new WaspSpecUserError(`Invalid \`${declType}\` reference: \`${potentialRef}\`\n` +
                `Please make sure that \`${potentialRef}\` is actually defined.`);
        }
        return {
            name: potentialRef,
            declType,
        };
    };
}
function makeConflictingDeclsError(existingDecl, incomingDecl) {
    return new WaspSpecUserError(`Conflicting configurations for the ${declTypeDisplayNames[existingDecl.declType]} \`${existingDecl.declValue.name}\`:\n\n` +
        `\`${existingDecl.declValue.name}\` (A):\n${showDecl(existingDecl)}\n\n` +
        `\`${incomingDecl.declValue.name}\` (B):\n${showDecl(incomingDecl)}\n\n` +
        `All definitions with the same name must produce the same configuration.\n` +
        "If the duplication was intentional, please use a different name to differentiate them.");
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
};
function showDecl(decl) {
    return JSON.stringify(decl.declValue, null, 2);
}
