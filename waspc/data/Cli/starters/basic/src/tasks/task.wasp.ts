import { action, query } from "@wasp.sh/spec";

export const tasksParts = [
  query(getTags, { entities: ["Tag"] }),
  action(createTag, { entities: ["Tag"] }),
];
