import { page, route, type Part } from "@wasp.sh/spec";

import { EagerPage } from "./pages/EagerPage" with { type: "ref" };
import { LazyPage } from "./pages/LazyPage" with { type: "ref" };

export const lazyLoading: Part[] = [
  route("EagerRoute", "/lazy/no", page(EagerPage), { lazy: false }),
  route("LazyRoute", "/lazy/yes", page(LazyPage)),
];
