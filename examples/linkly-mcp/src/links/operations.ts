// Wasp operations: thin adapters over service.ts. The dashboard calls them,
// and so does MCP, through the tools in mcp.ts.
import { prisma } from "wasp/server";
import type {
  CreateLink,
  DeleteLink,
  GetLink,
  GetLinks,
  UpdateLink,
} from "wasp/server/operations";
import type { Page } from "../shared/pagination";
import { runOperation } from "../shared/waspErrors";
import { callerPrincipal } from "../shared/waspSession";
import type { LinkInput, LinkListQuery, LinkOut, LinkUpdate } from "./schemas";
import * as links from "./service";

const deps = { prisma };

export const getLinks: GetLinks<LinkListQuery, Page<LinkOut>> = (
  args,
  context,
) =>
  runOperation(() =>
    links.listLinks(callerPrincipal(context), args ?? {}, deps),
  );

export const getLink: GetLink<{ id: string }, LinkOut> = (args, context) =>
  runOperation(() => links.getLink(callerPrincipal(context), args, deps));

export const createLink: CreateLink<LinkInput, LinkOut> = (args, context) =>
  runOperation(() => links.createLink(callerPrincipal(context), args, deps));

export const updateLink: UpdateLink<{ id: string } & LinkUpdate, LinkOut> = (
  args,
  context,
) => runOperation(() => links.updateLink(callerPrincipal(context), args, deps));

// The dashboard deletes directly after an in-UI confirm dialog: the human is
// already in the product, so no confirmation link is needed.
export const deleteLink: DeleteLink<
  { id: string },
  { status: "deleted"; id: string; slug: string }
> = (args, context) =>
  runOperation(() => links.deleteLink(callerPrincipal(context), args, deps));
