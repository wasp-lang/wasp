import type { Branded } from "./typeUtils";

export type AppName = Branded<string, "AppName">;
export type AppDirPath = Branded<string, "AppDirPath">;
export type AppParentDirPath = Branded<string, "AppParentDirPath">;
export type PatchesDirPath = Branded<string, "PatchesDirPath">;
export type TutorialDirPath = Branded<string, "TutorialDirPath">;
export type PatchFilePath = Branded<string, "PatchFilePath">;
export type StepId = Branded<string, "StepId">;
export type MarkdownFilePath = Branded<string, "MarkdownFilePath">;
