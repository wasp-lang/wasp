import type { Options } from "mdast-util-to-markdown";

declare module "mdast" {
  interface IdString extends Node {
    type: "idString";
    value: string;
  }

  interface RootContentMap {
    idString: IdString;
  }
}

export const customHeadingId: Options = {
  handlers: {
    idString: (node) => `{#${node.value}}`,
  },
};
