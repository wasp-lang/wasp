import { Module } from "wasp-config";

export type __waspModuleCamelName__Config = {};

export function __waspModuleCamelName__(config: __waspModuleCamelName__Config): Module {
  const mod = new Module("__waspModuleName__");

  // mod.entity("Example", {
  //   fields: {
  //     id: "Int @id @default(autoincrement())",
  //   },
  // });

  return mod;
}
