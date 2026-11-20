import { app, page, route } from "@wasp/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };

export default app({
  name: "waspApp",
  wasp: { version: "^0.27.0" },
  title: "wasp-app",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  spec: [
    route("RootRoute", "/", page(MainPage)),
  ],
});
