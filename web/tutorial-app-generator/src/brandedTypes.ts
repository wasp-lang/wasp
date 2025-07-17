import type { Branded } from "./typeUtils";

export type AppName = Branded<string, "AppName">;
export type AppDirPath = Branded<string, "AppDirPath">;
export type PatchesDirPath = Branded<string, "PatchesDirPath">;
export type PatchContentPath = Branded<string, "PatchContentPath">;
