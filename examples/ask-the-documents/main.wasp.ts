import { action, app, page, query, route } from "@wasp.sh/spec";

import {
  getGoogleAuthConfig,
  googleUserSignupFields,
} from "./src/auth/google" with { type: "ref" };
import {
  askDocuments,
  deleteAllDocuments,
  deleteDocument,
  embedDocument,
  getDocuments,
  getScrapeCandidates,
  searchDocuments,
} from "./src/documents" with { type: "ref" };
import { serverEnvValidation } from "./src/env" with { type: "ref" };
import { Layout } from "./src/Layout" with { type: "ref" };
import { Main } from "./src/pages/MainPage" with { type: "ref" };

const rootRoute = route("RootRoute", "/", page(Main), { prerender: true });

export default app({
  name: "askTheDocuments",
  wasp: { version: "0.25.0" },
  title: "PG Vector Example",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    userEntity: "User",
    methods: {
      google: {
        userSignupFields: googleUserSignupFields,
        configFn: getGoogleAuthConfig,
      },
    },
    onAuthFailedRedirectTo: rootRoute,
  },
  client: {
    rootComponent: Layout,
  },
  server: {
    envValidationSchema: serverEnvValidation,
  },
  spec: [
    rootRoute,

    action(embedDocument, { entities: ["Document"] }),
    action(getScrapeCandidates, { entities: ["Document"] }),
    query(getDocuments, { entities: ["Document"] }),
    action(searchDocuments, { entities: ["Document"] }),
    action(askDocuments, { entities: ["Document"] }),
    action(deleteDocument, { entities: ["Document"] }),
    action(deleteAllDocuments, { entities: ["Document"] }),
  ],
});
