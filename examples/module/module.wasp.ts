import { action, page, query, route, type Spec } from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { addRandomTodo } from "./src/actions" with { type: "ref" };
import { getTodoItems } from "./src/queries" with { type: "ref" };

type ModuleOptions = {
  prefix: string;
};

export default function getModuleSpec(options: ModuleOptions): Spec {
  return [
    route(
      "ModuleRoute",
      options.prefix,
      page(MainPage, { authRequired: true }),
    ),
    query(getTodoItems, { entities: ["Task"] }),
    action(addRandomTodo, { entities: ["Task"] }),
  ];
}
