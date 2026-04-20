export type ExtImport =
  | { import: string; alias?: string; from: `@src/${string}` }
  | { importDefault: string; from: `@src/${string}` };

export type Page = {
  part: "page";
  component: ExtImport;
  authRequired?: boolean;
};

export type Route = {
  part: "route";
  path: string;
  page: Page | ExtImport;
};

export type Query = {
  part: "query";
  fn: ExtImport;
  entities?: string[];
  auth?: boolean;
};

export type Part = Page | Route | Query;
