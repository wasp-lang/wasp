import { api, apiNamespace, page, route } from "@wasp.sh/spec";

import {
  bar,
  barBaz,
  fooBar,
  fooBarMiddlewareFn,
  webhookCallback,
  webhookCallbackMiddlewareFn,
} from "./apis" with { type: "ref" };
import { ApisPage } from "./pages/ApisPage" with { type: "ref" };

export const apis = [
  route("ApisRoute", "/apis", page(ApisPage)),
  api("ALL", "/foo/bar", fooBar, {
    middlewareConfigFn: fooBarMiddlewareFn,
    entities: ["Task"],
  }),
  apiNamespace("/bar", {
    middlewareConfigFn: bar,
  }),
  api("GET", "/bar/baz", barBaz, { auth: false, entities: ["Task"] }),
  api("POST", "/webhook/callback", webhookCallback, {
    middlewareConfigFn: webhookCallbackMiddlewareFn,
    auth: false,
  }),
];
