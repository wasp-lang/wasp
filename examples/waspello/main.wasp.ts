// @ts-ignore
import LoginPage from "@src/auth/LoginPage";
// @ts-ignore
import SignupPage from "@src/auth/SignupPage";
// @ts-ignore
import { createCard, updateCard } from "@src/cards/cards";
import {
  createList,
  createListCopy,
  deleteList,
  getListsAndCards,
  updateList, // @ts-ignore
} from "@src/cards/lists";
// @ts-ignore
import MainPage from "@src/cards/MainPage";
// @ts-ignore
import Layout from "@src/Layout";
import { readFile } from "fs/promises";
import { action, app, page, query, route } from "wasp-config";

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
    route("SignupRoute", "/signup", page(SignupPage)),
    route("LoginRoute", "/login", page(LoginPage)),
    query(getListsAndCards, { entities: ["List", "Card"] }),
    action(createList, { entities: ["List"] }),
    action(updateList, { entities: ["List"] }),
    action(deleteList, { entities: ["List", "Card"] }),
    action(createListCopy, { entities: ["List", "Card"] }),
    action(createCard, { entities: ["Card"] }),
    action(updateCard, { entities: ["Card"] }),
  ],
});
