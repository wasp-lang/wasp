import { app, page, route } from "@wasp.sh/spec";

import { App } from "./src/App" with { type: "ref" };
import { clientSetup } from "./src/clientSetup" with { type: "ref" };
import {
  clientEnvValidationSchema,
  serverEnvValidationSchema,
} from "./src/env" with { type: "ref" };
import { CatchAllPage } from "./src/pages/CatchAllPage" with { type: "ref" };
import {
  serverMiddlewareFn,
  serverSetup,
} from "./src/serverSetup" with { type: "ref" };

import { apisSpec } from "./src/features/apis/apis.wasp";
import { authConfig, authSpec } from "./src/features/auth/auth.wasp";
import { chatSpec, webSocket } from "./src/features/chat/chat.wasp";
import { crudSpec } from "./src/features/crud/crud.wasp";
import { db } from "./src/features/db/db.wasp";
import { jobsSpec } from "./src/features/jobs/jobs.wasp";
import { lazyLoadingSpec } from "./src/features/lazy-loading/lazyLoading.wasp";
import { operationsSpec } from "./src/features/operations/operations.wasp";
import { prerenderSpec } from "./src/features/prerender/prerender.wasp";
import { streamingSpec } from "./src/features/streaming/streaming.wasp";
import { homeRoute } from "./src/home.wasp";
import { rpcTestsSpec } from "./src/rpcTests/rpcTests.wasp";

export default app({
  name: "KitchenSink",
  wasp: { version: "0.25.0" },
  title: "Wasp Kitchen Sink",
  head: [
    "<link rel='manifest' href='/manifest.json' />",
    "<link rel='icon' href='/favicon.ico' />",
  ],
  webSocket,
  auth: authConfig,
  server: {
    setupFn: serverSetup,
    middlewareConfigFn: serverMiddlewareFn,
    envValidationSchema: serverEnvValidationSchema,
  },
  client: {
    rootComponent: App,
    setupFn: clientSetup,
    envValidationSchema: clientEnvValidationSchema,
  },
  db,
  emailSender: {
    provider: "SMTP",
    defaultFrom: {
      email: "kitchen-sink@wasp.sh",
    },
  },
  spec: [
    homeRoute,
    route("CatchAllRoute", "*", page(CatchAllPage)),
    authSpec,
    operationsSpec,
    jobsSpec,
    apisSpec,
    crudSpec,
    streamingSpec,
    chatSpec,
    lazyLoadingSpec,
    prerenderSpec,
    rpcTestsSpec,
  ],
});
