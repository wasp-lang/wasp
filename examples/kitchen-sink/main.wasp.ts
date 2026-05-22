import { app, page, route } from "@wasp.sh/spec";

import { App } from "./src/App" with { type: "ref" };
import { clientSetup } from "./src/clientSetup" with { type: "ref" };
import {
  clientEnvValidationSchema,
  serverEnvValidationSchema,
} from "./src/env" with { type: "ref" };
import { CatchAllPage } from "./src/pages/CatchAllPage" with { type: "ref" };
import { HomePage } from "./src/pages/HomePage" with { type: "ref" };
import {
  serverMiddlewareFn,
  serverSetup,
} from "./src/serverSetup" with { type: "ref" };

import { apis } from "./src/features/apis/apis.wasp";
import { auth, authConfig } from "./src/features/auth/auth.wasp";
import { chat, webSocket } from "./src/features/chat/chat.wasp";
import { crudFeature } from "./src/features/crud/crud.wasp";
import { db } from "./src/features/db/db.wasp";
import { jobs } from "./src/features/jobs/jobs.wasp";
import { lazyLoading } from "./src/features/lazy-loading/lazyLoading.wasp";
import { operations } from "./src/features/operations/operations.wasp";
import { prerender } from "./src/features/prerender/prerender.wasp";
import { streaming } from "./src/features/streaming/streaming.wasp";
import { rpcTests } from "./src/rpcTests/rpcTests.wasp";

export default app({
  name: "KitchenSink",
  wasp: { version: "^0.24.0" },
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
  parts: [
    route("HomeRoute", "/", page(HomePage), { prerender: true }),
    route("CatchAllRoute", "*", page(CatchAllPage)),

    ...auth,
    ...operations,
    ...jobs,
    ...apis,
    ...crudFeature,
    ...streaming,
    ...chat,
    ...lazyLoading,
    ...prerender,
    ...rpcTests,
  ],
});
