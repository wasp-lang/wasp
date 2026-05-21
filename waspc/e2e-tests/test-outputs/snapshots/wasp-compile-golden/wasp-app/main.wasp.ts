import { MainPage } from "@src/MainPage"
import { app, page, route } from "@wasp.sh/spec"

export default app({
  name: "waspApp",
  title: "wasp-app",
  wasp: { version: "^0.24.0" },
  head: ["<link rel='icon' href='/favicon.ico' />"],
  parts: [
    route("RootRoute", "/", page(MainPage)),
  ],
});
