import type { QueryFn } from "wasp/server/types";

type ModuleContent = {
  message: string;
  servedAt: string;
};

export const getModuleContent: QueryFn<void, ModuleContent> = async () => {
  return {
    message: "This content was loaded from a query shipped by the module.",
    servedAt: new Date().toISOString(),
  };
};
