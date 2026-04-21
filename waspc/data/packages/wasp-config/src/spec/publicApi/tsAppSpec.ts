export type TsAppSpec = {
  kind: "app";
  name: string;
  wasp: { version: string };
  title: string;
  head?: string[];
  parts: Part[];
};

export type Part = Page | Query;

export type Page = {
  kind: "page";
  component: ExtImport;
  authRequired?: boolean;
};

export type Query = {
  kind: "query";
  fn: ExtImport;
  entities?: string[];
  auth?: boolean;
};

export type ExtImport =
  | { import: string; alias?: string; from: `@src/${string}` }
  | { importDefault: string; from: `@src/${string}` };
