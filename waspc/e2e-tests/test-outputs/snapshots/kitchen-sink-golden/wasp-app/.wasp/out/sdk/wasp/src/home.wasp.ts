import { page, route } from "@wasp.sh/spec";

import { HomePage } from "./pages/HomePage" with { type: "ref" };

export const homeRoute = route("HomeRoute", "/", page(HomePage), {
  prerender: true,
});
