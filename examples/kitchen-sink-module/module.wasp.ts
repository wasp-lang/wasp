import {
  action,
  api,
  apiNamespace,
  crud,
  job,
  page,
  query,
  route,
  type Spec,
} from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { addRandomTodo } from "./src/actions" with { type: "ref" };
import { moduleApiNamespaceMiddlewareFn } from "./src/apiNamespaceMiddleware" with { type: "ref" };
import { getAllModuleTodos } from "./src/crud" with { type: "ref" };
import {
  getModuleApiPrefix,
  getModuleJobApiPath,
  getModulePingApiPath,
} from "./src/moduleApiContract";
import {
  handleModulePing,
  moduleJob,
  startModuleJob,
} from "./src/moduleApiServer" with { type: "ref" };
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
    crud("moduleTodos", "Task", {
      getAll: { overrideFn: getAllModuleTodos },
      update: {},
      delete: {},
    }),
    apiNamespace(getModuleApiPrefix(options.prefix), {
      middlewareConfigFn: moduleApiNamespaceMiddlewareFn,
    }),
    api("GET", getModulePingApiPath(options.prefix), handleModulePing, {
      auth: false,
    }),
    api("POST", getModuleJobApiPath(options.prefix), startModuleJob, {
      auth: false,
    }),
    job(moduleJob, { executor: "PgBoss" }),
  ];
}
