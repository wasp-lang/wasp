import { app, page, route } from "wasp-config";
import { MainPage } from "@src/MainPage";

export default app({
  name: "__waspAppName__",
  title: "__waspProjectName__",
  wasp: { version: "__waspVersion__" },
  head: ["<link rel='icon' href='/favicon.ico' />"],
  parts: [
    route("RootRoute", "/", page(MainPage)),
  ],
});
