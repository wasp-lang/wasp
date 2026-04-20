export type TsAppSpec = {
  part: "app";
  name: string;
  wasp: { version: string };
  title: string;
  head?: string[];
  parts: Part[];
};

export type ExtImport =
  | { import: string; alias?: string; from: `@src/${string}` }
  | { importDefault: string; from: `@src/${string}` };

export type Page = {
  part: "page";
  component: ExtImport;
  authRequired?: boolean;
};

export type Query = {
  part: "query";
  fn: ExtImport;
  entities?: string[];
  auth?: boolean;
};

export type Part = Page | Query;
