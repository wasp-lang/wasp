import type { Query } from "wasp/server/operations";

type ModuleContent = {
  message: string;
  servedAt: string;
};

export const getModuleContent: Query<void, ModuleContent> = async () => {
  return {
    message: "This content was loaded from a query shipped by the module.",
    servedAt: new Date().toISOString(),
  };
};
