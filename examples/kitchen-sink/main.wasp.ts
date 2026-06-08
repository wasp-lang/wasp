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

import { apisDecls } from "./src/features/apis/apis.wasp";
import { authConfig, authDecls } from "./src/features/auth/auth.wasp";
import { chatDecls, webSocket } from "./src/features/chat/chat.wasp";
import { crudFeatureDecls } from "./src/features/crud/crud.wasp";
import { db } from "./src/features/db/db.wasp";
import { jobsDecls } from "./src/features/jobs/jobs.wasp";
import { lazyLoadingDecls } from "./src/features/lazy-loading/lazyLoading.wasp";
import { operationsDecls } from "./src/features/operations/operations.wasp";
import { prerenderDecls } from "./src/features/prerender/prerender.wasp";
import { streamingDecls } from "./src/features/streaming/streaming.wasp";
import { rpcTestsDecls } from "./src/rpcTests/rpcTests.wasp";

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
  decls: [
    route("HomeRoute", "/", page(HomePage), { prerender: true }),
    route("CatchAllRoute", "*", page(CatchAllPage)),

    ...authDecls,
    ...operationsDecls,
    ...jobsDecls,
    ...apisDecls,
    ...crudFeatureDecls,
    ...streamingDecls,
    ...chatDecls,
    ...lazyLoadingDecls,
    ...prerenderDecls,
    ...rpcTestsDecls,
  ],
});
