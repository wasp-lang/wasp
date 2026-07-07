import type { GetModuleContent } from "wasp/server/operations";

type ModuleContent = {
  message: string;
  servedAt: string;
};

export const getModuleContent: GetModuleContent<
  void,
  ModuleContent
> = async () => {
  return {
    message: "This content was loaded from a query shipped by the module.",
    servedAt: new Date().toISOString(),
  };
};
