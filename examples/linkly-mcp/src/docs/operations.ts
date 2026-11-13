// The docs tools' operations. Only MCP calls them: with `expose.mcp` they'd
// set `expose.client: false`, so no client route or export is generated.
import type { ReadDoc, SearchDocs } from "wasp/server/operations";
import { runOperation } from "../shared/waspErrors";
import type {
  DocOut,
  DocSearchResults,
  ReadDocInput,
  SearchDocsInput,
} from "./schemas";
import * as docs from "./service";

export const searchDocs: SearchDocs<SearchDocsInput, DocSearchResults> = (
  args,
) => runOperation(async () => docs.searchDocs(args));

export const readDoc: ReadDoc<ReadDocInput, DocOut> = (args) =>
  runOperation(async () => docs.readDoc(args));
