import { type Spec, action, query } from "@wasp/spec";
import { createTag } from "./actions" with { type: "ref" };
import { getTags } from "./queries" with { type: "ref" };

export const tagsSpec: Spec = [
  query(getTags, { entities: ["Tag"] }),
  action(createTag, { entities: ["Tag"] }),
];
