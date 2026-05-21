import { app, page, route } from "@wasp.sh/spec";
import { readFile } from "fs/promises";
import MainPage from "./src/cards/MainPage" with { type: "ref" };
import Layout from "./src/Layout" with { type: "ref" };

import { auth } from "./src/auth/auth.wasp";
import { cards } from "./src/cards/cards.wasp";

export default app({
  name: "waspello",
  title: (await readFile("appTitle.txt", "utf-8")).trim(),
  wasp: { version: "^0.24.0" },
  auth: {
    userEntity: "User",
    methods: {
      usernameAndPassword: {},
      google: {},
    },
    onAuthFailedRedirectTo: "/login",
  },
  client: {
    rootComponent: Layout,
  },
  parts: [
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    ...auth,
    ...cards,
  ],
});
