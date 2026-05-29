import { api, apiNamespace, page, route, type Decl } from "@wasp.sh/spec";

import {
  barBaz,
  barNamespaceMiddlewareFn,
  fooBar,
  fooBarMiddlewareFn,
  webhookCallback,
  webhookCallbackMiddlewareFn,
} from "./apis" with { type: "ref" };
import { ApisPage } from "./pages/ApisPage" with { type: "ref" };

export const apis: Decl[] = [
  route("ApisRoute", "/apis", page(ApisPage)),
  api("ALL", "/foo/bar", fooBar, {
    middlewareConfigFn: fooBarMiddlewareFn,
    entities: ["Task"],
  }),
  apiNamespace("/bar", {
    middlewareConfigFn: barNamespaceMiddlewareFn,
  }),
  api("GET", "/bar/baz", barBaz, { auth: false, entities: ["Task"] }),
  api("POST", "/webhook/callback", webhookCallback, {
    middlewareConfigFn: webhookCallbackMiddlewareFn,
    auth: false,
  }),
];
