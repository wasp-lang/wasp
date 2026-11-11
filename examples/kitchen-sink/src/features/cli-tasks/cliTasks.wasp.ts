import { task, type Spec } from "@wasp/spec";

import { deleteDoneTasks } from "./deleteDoneTasks" with { type: "ref" };

export const cliTasksSpec: Spec = [
  task(deleteDoneTasks, {
    entities: ["Task"],
    description: "Deletes all done tasks. Pass --dry-run to only count them.",
  }),
];
