import { getModuleSpec } from "./_private.js";
import * as AppSpec from "./appSpec.js";
import { mapTsModuleSpecToModuleSpecOutput } from "./mapTsAppSpecToAppSpecDecls.js";
import { Module } from "./publicApi/Module.js";

type Result<Value, Error> =
  | { status: "ok"; value: Value }
  | { status: "error"; error: Error };

export async function analyzeModule(
  module: Module,
): Promise<Result<AppSpec.ModuleSpecOutput, string>> {
  try {
    const moduleSpec = getModuleSpec(module);
    const output = mapTsModuleSpecToModuleSpecOutput(moduleSpec);
    return { status: "ok", value: output };
  } catch (e) {
    const message = e instanceof Error ? e.message : String(e);
    return { status: "error", error: message };
  }
}
