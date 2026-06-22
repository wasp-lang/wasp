import { page, route, type Spec } from "@wasp.sh/spec";

import { HydrationMismatchPage } from "./pages/HydrationMismatchPage" with { type: "ref" };
import { PrerenderInstancesPage } from "./pages/PrerenderInstancesPage" with { type: "ref" };
import { PrerenderPage } from "./pages/PrerenderPage" with { type: "ref" };

export const prerenderSpec: Spec = [
  route("PrerenderRoute", "/prerender", page(PrerenderPage), {
    prerender: true,
  }),
  // A dynamic route that prerenders specific concrete instances at build time.
  route(
    "PrerenderInstancesRoute",
    "/prerender-instances/:slug",
    page(PrerenderInstancesPage),
    {
      prerender: ["/prerender-instances/martin", "/prerender-instances/matija"],
    },
  ),
  route(
    "HydrationMismatchRoute",
    "/hydration-mismatch",
    page(HydrationMismatchPage),
    { prerender: true },
  ),
];
