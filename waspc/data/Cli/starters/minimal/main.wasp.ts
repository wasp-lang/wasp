import { app, page, route } from "@wasp/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };

export default app({
  name: "__waspAppName__",
  title: "__waspProjectName__",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  spec: [
    route("RootRoute", "/", page(MainPage)),
  ],
});
