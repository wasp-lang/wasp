import * as AppSpec from "../../appSpec.js";
import * as WaspSpec from "../publicApi/waspSpec.js";
import { AppSpecDeclTypeForWaspSpecElement } from "./specElements.js";

export { AppSpec, WaspSpec };

export type WaspSpecElementKind = WaspSpec.SpecElement["kind"];
export type AppSpecDeclType = AppSpec.Decl["declType"];

export type WaspSpecElementForKind<Kind extends WaspSpecElementKind> = Extract<
  WaspSpec.SpecElement,
  { kind: Kind }
>;

export type AppSpecElementForType<Type extends AppSpecDeclType> =
  AppSpec.GetDeclForType<Type>;

export type GetSpecElementForKind<Kind extends WaspSpec.SpecElement["kind"]> =
  Extract<WaspSpec.SpecElement, { kind: Kind }>;

export interface AppMapperContext {
  emitEntityRef(name: string): AppSpec.Ref<"Entity">;

  emitRefObject(refObject: WaspSpec.Reference<unknown>): AppSpec.ExtImport;

  emitSpecElementRef<SpecElement extends WaspSpec.SpecElement>(
    specElement: SpecElement,
  ): AppSpec.Ref<AppSpecDeclTypeForWaspSpecElement<SpecElement>>;
}
