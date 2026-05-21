import { page, route } from "@wasp.sh/spec";

import { HydrationMismatchPage } from "./pages/HydrationMismatchPage" with { type: "ref" };
import { PrerenderPage } from "./pages/PrerenderPage" with { type: "ref" };

export const prerender = [
  route("PrerenderRoute", "/prerender", page(PrerenderPage), {
    prerender: true,
  }),
  route(
    "HydrationMismatchRoute",
    "/hydration-mismatch",
    page(HydrationMismatchPage),
    { prerender: true },
  ),
];
