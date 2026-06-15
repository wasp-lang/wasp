import { app, page, route } from "@wasp.sh/spec";
import { readFile } from "fs/promises";
import MainPage from "./src/cards/MainPage" with { type: "ref" };
import Layout from "./src/Layout" with { type: "ref" };

import { authSpec } from "./src/auth/auth.wasp";
import { cardsSpec } from "./src/cards/cards.wasp";

export default app({
  name: "waspello",
  wasp: { version: "^0.25.0" },
  title: (await readFile("appTitle.txt", "utf-8")).trim(),
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
  spec: [
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    authSpec,
    cardsSpec,
  ],
});
