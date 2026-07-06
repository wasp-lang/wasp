import { page, route } from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };

export const moduleSpec = [route("ModuleRoute", "/fsm", page(MainPage))];
