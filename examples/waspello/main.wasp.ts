import { app, page, route } from "wasp-config";

import { auth } from "./src/auth/auth.wasp";
import { cards } from "./src/cards/cards.wasp";

import MainPage from "@src/cards/MainPage";
import Layout from "@src/Layout";
import { readFile } from "fs/promises";

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
