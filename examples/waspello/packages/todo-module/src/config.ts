import { config } from "wasp/modules/config";
import { PACKAGE_NAME } from "./index.js";

type RuntimeConfig = {
  todoEntityName: string;
  userForeignKey?: string;
};

export const moduleConfig = config.get(PACKAGE_NAME)! as RuntimeConfig;
