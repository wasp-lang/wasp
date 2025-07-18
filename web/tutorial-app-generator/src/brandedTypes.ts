import type { Branded } from "./typeUtils";

export type AppName = Branded<string, "AppName">;
export type AppDirPath = Branded<string, "AppDirPath">;
export type PatchesDirPath = Branded<string, "PatchesDirPath">;
export type TutorialDirPath = Branded<string, "TutorialDirPath">;
export type PatchFilePath = Branded<string, "PatchFilePath">;
export type StepName = Branded<string, "StepName">;
export type MarkdownFilePath = Branded<string, "MarkdownFilePath">;
