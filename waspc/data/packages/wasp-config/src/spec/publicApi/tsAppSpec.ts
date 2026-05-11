import type { ExtImport as SpecExtImport } from "../extImport.js";

export type App = {
  name: string;
  wasp: { version: string };
  title: string;
  head?: string[];
  parts: Part[];
};

export type Part = Page | Route | Query | Action;

export type Page = MakePart<
  "page",
  {
    component: SpecExtImport;
    authRequired?: boolean;
  }
>;

export type Route = MakePart<
  "route",
  {
    name: string;
    path: string;
    page: Page;
    prerender?: boolean;
    lazy?: boolean;
  }
>;

export type Query = MakePart<
  "query",
  {
    fn: SpecExtImport;
    entities?: string[];
    auth?: boolean;
  }
>;

export type Action = MakePart<
  "action",
  {
    fn: SpecExtImport;
    entities?: string[];
    auth?: boolean;
  }
>;

export type {
  DefaultExtImport,
  ExtImport,
  NamedExtImport,
} from "../extImport.js";

/**
 * We need the kind to differentiate between parts with the same structure. One
 * example is queries and actions.
 *
 * Imagine the situation if we didn't have kinds. How would TS know which one
 * is an action and which one a query?
 *   { fn: { import: "getFoo" from: "@src/foo" }, auth: true }
 *   { fn: { import: "setFoo" from: "@src/foo" }, auth: true }
 *
 * There are likely other examples where some combination of mandatory and
 * optional fields makes two parts structurally identical.
 */
export type MakePart<Kind extends string, PartConfig> = {
  kind: Kind;
} & PartConfig;
