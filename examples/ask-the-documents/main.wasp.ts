import { action, app, page, query, route } from "@wasp/spec";

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

export default app({
  name: "askTheDocuments",
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
    onAuthFailedRedirectTo: "/",
  },
  client: {
    rootComponent: Layout,
  },
  server: {
    envValidationSchema: serverEnvValidation,
  },
  spec: [
    route("RootRoute", "/", page(Main), { prerender: true }),

    action(embedDocument, { entities: ["Document"] }),
    action(getScrapeCandidates, { entities: ["Document"] }),
    query(getDocuments, { entities: ["Document"] }),
    action(searchDocuments, { entities: ["Document"] }),
    action(askDocuments, { entities: ["Document"] }),
    action(deleteDocument, { entities: ["Document"] }),
    action(deleteAllDocuments, { entities: ["Document"] }),
  ],
});
