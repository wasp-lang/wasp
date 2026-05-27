import { crud, page, route, type Decl } from "@wasp.sh/spec";

import { crudCreateTask, crudGetAllTasks } from "./crud" with { type: "ref" };
import { DetailPage } from "./pages/DetailPage" with { type: "ref" };
import { ListPage } from "./pages/ListPage" with { type: "ref" };

export const crudFeature: Decl[] = [
  crud("tasks", "Task", {
    get: {},
    getAll: {
      overrideFn: crudGetAllTasks,
    },
    create: {
      overrideFn: crudCreateTask,
    },
    update: {},
    delete: {},
  }),
  route("CrudListRoute", "/crud", page(ListPage, { authRequired: true })),
  route(
    "CrudDetailRoute",
    "/crud/:id",
    page(DetailPage, { authRequired: true }),
  ),
];
