import * as AppSpec from "../../appSpec.js";
import { Branded } from "../../branded.js";

export type TsAppSpec = {
  app: { name: string; config: AppConfig };
  pages: Map<string, PageConfig>;
  queries: Map<string, QueryConfig>;
};

export type AppConfig = {
  title: string;
  wasp: AppSpec.Wasp;
  head?: string[];
};

export type PageConfig = {
  component: ExtImport;
  authRequired?: boolean;
};

export type QueryConfig = {
  fn: ExtImport;
  entities?: string[];
  auth?: boolean;
};

export type PageName = Branded<string, "PageName">;

export type ExtImport =
  | {
      import: string;
      from: AppSpec.ExtImport["path"];
    }
  | {
      importDefault: string;
      from: AppSpec.ExtImport["path"];
    };
