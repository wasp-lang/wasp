// Maps a component's JSX name to its markdown renderer. Consulted first by the
// flatten in render-mdx.ts. This module must stay React-free (it runs in Node at
// build time), so it only imports the `*.markdown.ts` renderer modules.

import { CardLink } from "../components/CardLink/markdown";
import { FileExtSwitcher } from "../components/FileExtSwitcher/markdown";
import { LinkGrid } from "../components/LinkGrid/markdown";
import * as Tag from "../components/Tag/markdown";
import * as TsJs from "../components/TsJsHelpers/markdown";
import type { MarkdownRenderer } from "./doc-component";
import { externalRenderers } from "./external-renderers";

export const markdownRegistry: Record<string, MarkdownRenderer> = {
  ShowForTs: TsJs.ShowForTs,
  ShowForJs: TsJs.ShowForJs,
  LinkGrid,
  CardLink,
  FileExtSwitcher,
  Internal: Tag.Internal,
  Required: Tag.Required,
  Optional: Tag.Optional,
  ...externalRenderers,
};
