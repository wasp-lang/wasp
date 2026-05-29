import { type Decl, action, query } from "@wasp.sh/spec";
import { createTag } from "./actions" with { type: "ref" };
import { getTags } from "./queries" with { type: "ref" };

export const tags: Decl[] = [
  query(getTags, { entities: ["Tag"] }),
  action(createTag, { entities: ["Tag"] }),
];
