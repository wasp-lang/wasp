import { Module } from "wasp-config/module";

export const PACKAGE_NAME = "@waspello/todo-module";

export type TodoModuleConfig = {
  todoEntityName: string;
  route: string;
  cleanDoneTodosCron?: string;
  userEntityName?: string;
  userForeignKey?: string;
};

export function createTodoModule(config: TodoModuleConfig): Module {
  const mod = new Module(PACKAGE_NAME);
  const { todoEntityName, userEntityName, userForeignKey } = config;
  const useAuth = !!userEntityName;

  mod.provide("todoEntityName", todoEntityName);
  if (userForeignKey) {
    mod.provide("userForeignKey", userForeignKey);
  }

  const entities = userEntityName
    ? [todoEntityName, userEntityName]
    : [todoEntityName];

  // page + route
  const todosPage = mod.page("Todos", {
    component: { importDefault: "TodoPage", from: "@src/TodoPage" },
    ...(useAuth && { authRequired: true }),
  });
  mod.route("TodosRoute", { path: config.route, to: todosPage });

  // query
  mod.query("getTodos", {
    fn: { import: "getTodos", from: "@src/queries" },
    entities,
    ...(useAuth && { auth: true }),
  });

  // actions
  mod.action("createTodo", {
    fn: { import: "createTodo", from: "@src/actions" },
    entities,
    ...(useAuth && { auth: true }),
  });
  mod.action("updateTodo", {
    fn: { import: "updateTodo", from: "@src/actions" },
    entities,
    ...(useAuth && { auth: true }),
  });
  mod.action("deleteTodo", {
    fn: { import: "deleteTodo", from: "@src/actions" },
    entities,
    ...(useAuth && { auth: true }),
  });

  // api + apiNamespace
  mod.api("getTodosApi", {
    fn: { import: "getTodosApi", from: "@src/apis" },
    httpRoute: { method: "GET", route: "/api/todos" },
    entities,
    auth: useAuth,
  });
  mod.api("getTodoStatsApi", {
    fn: { import: "getTodoStatsApi", from: "@src/apis" },
    httpRoute: { method: "GET", route: "/api/todos/stats" },
    entities,
    auth: useAuth,
  });
  mod.apiNamespace("todosNamespace", {
    middlewareConfigFn: {
      import: "todoApiMiddleware",
      from: "@src/middleware",
    },
    path: "/api/todos",
  });

  // crud
  mod.crud("todoCrud", {
    entity: todoEntityName,
    operations: {
      getAll: {
        isPublic: !useAuth,
        overrideFn: { import: "getAllOverride", from: "@src/crud" },
      },
      get: { isPublic: !useAuth },
    },
  });

  // job
  mod.job("cleanDoneTodosJob", {
    executor: "PgBoss",
    perform: { fn: { import: "cleanDoneTodos", from: "@src/jobs" } },
    schedule: { cron: config.cleanDoneTodosCron ?? "0 * * * *" },
    entities,
  });

  // serverSetup + clientSetup
  mod.serverSetup({ import: "initServer", from: "@src/serverSetup" });
  mod.clientSetup({ import: "initClient", from: "@src/clientSetup" });

  return mod;
}
