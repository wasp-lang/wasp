import { expect, test } from "vitest";
import { routes } from "wasp/client/router";

test("route builder keeps a numeric 0 param", () => {
  expect(routes.TaskRoute.build({ params: { id: 0 } })).toBe("/tasks/0");
});

test("route builder interpolates other numeric params", () => {
  expect(routes.TaskRoute.build({ params: { id: 42 } })).toBe("/tasks/42");
});
