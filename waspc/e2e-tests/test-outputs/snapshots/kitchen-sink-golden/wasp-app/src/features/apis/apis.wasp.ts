import { api, apiNamespace, page, route, type Spec } from "@wasp.sh/spec";

import {
  barBaz,
  barNamespaceMiddlewareFn,
  fooBar,
  fooBarMiddlewareFn,
  headBarBaz,
  optionsBarBaz,
  optionsBarBazMiddlewareFn,
  patchBarBaz,
  webhookCallback,
  webhookCallbackMiddlewareFn,
} from "./apis" with { type: "ref" };
import { ApisPage } from "./pages/ApisPage" with { type: "ref" };

export const apisSpec: Spec = [
  route("ApisRoute", "/apis", page(ApisPage)),
  api("ALL", "/foo/bar", fooBar, {
    middlewareConfigFn: fooBarMiddlewareFn,
    entities: ["Task"],
  }),
  apiNamespace("/bar", {
    middlewareConfigFn: barNamespaceMiddlewareFn,
  }),
  // A HEAD api has to come before the GET api on the same path, otherwise the
  // GET api answers HEAD requests.
  api("HEAD", "/bar/baz", headBarBaz, { auth: false }),
  api("GET", "/bar/baz", barBaz, { auth: false, entities: ["Task"] }),
  api("PATCH", "/bar/baz", patchBarBaz, { auth: false }),
  api("OPTIONS", "/bar/baz", optionsBarBaz, {
    auth: false,
    middlewareConfigFn: optionsBarBazMiddlewareFn,
  }),
  api("POST", "/webhook/callback", webhookCallback, {
    middlewareConfigFn: webhookCallbackMiddlewareFn,
    auth: false,
  }),
];
