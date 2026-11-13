import { action, page, query, route, type Spec } from "@wasp/spec";
import {
  createLink,
  deleteLink,
  getLink,
  getLinks,
  updateLink,
} from "./operations" with { type: "ref" };
import { describeDeleteLink } from "./describeDeleteLink" with { type: "ref" };
import {
  CreateLinkInput,
  DeleteLinkInput,
  DeleteLinkOut,
  GetLinkInput,
  LinkOut,
  LinkPage,
  ListLinksInput,
  UpdateLinkInput,
} from "./schemas" with { type: "ref" };
import { LinkDetailPage } from "./ui/LinkDetailPage" with { type: "ref" };
import { LinksPage } from "./ui/LinksPage" with { type: "ref" };
import { NewLinkPage } from "./ui/NewLinkPage" with { type: "ref" };

export const linksSpec: Spec = [
  // Dashboard
  route("LinksRoute", "/links", page(LinksPage, { authRequired: true })),
  route(
    "NewLinkRoute",
    "/links/new",
    page(NewLinkPage, { authRequired: true }),
  ),
  route(
    "LinkDetailRoute",
    "/links/:id",
    page(LinkDetailPage, { authRequired: true }),
  ),
  query(getLinks, {
    entities: ["Link"],
    expose: {
      mcp: {
        // The default would be get_links.
        name: "list_links",
        title: "List links",
        description:
          "Lists the user's short links, newest first. Filter with `search` (matches slug, title, and destination) or `tag`. Results are paginated: pass `nextCursor` back as `cursor` for more. Use get_link for one link and get_link_stats for its clicks.",
        input: ListLinksInput,
        output: LinkPage,
        idempotent: true,
        scope: "links:read",
      },
    },
  }),
  query(getLink, {
    entities: ["Link"],
    expose: {
      mcp: {
        title: "Get a link",
        description:
          "Gets one short link by its ID or its slug (either works). Returns the destination, tags, short URL, and a dashboardUrl to open it in Linkly. Use list_links to find links, get_link_stats for clicks.",
        input: GetLinkInput,
        output: LinkOut,
        idempotent: true,
        scope: "links:read",
      },
    },
  }),
  action(createLink, {
    entities: ["Link"],
    expose: {
      mcp: {
        title: "Create a link",
        description:
          "Creates a short link that redirects to `destinationUrl`. Omit `slug` for a random 7-character one; a taken slug returns a conflict error. Returns the new link with its `shortUrl` to share. To change it later use update_link.",
        input: CreateLinkInput,
        output: LinkOut,
        scope: "links:write",
      },
    },
  }),
  action(updateLink, {
    entities: ["Link"],
    expose: {
      mcp: {
        title: "Update a link",
        description:
          "Changes a link's destination, slug, title, tags, or archived state. Only the fields you pass change; `tags` replaces the whole list. Archiving (archived: true) stops the short URL from redirecting without deleting it; prefer it over delete_link when the user might want the link back.",
        input: UpdateLinkInput,
        output: LinkOut,
        idempotent: true,
        scope: "links:write",
      },
    },
  }),
  action(deleteLink, {
    entities: ["Link"],
    expose: {
      mcp: {
        title: "Delete a link",
        description:
          "Permanently deletes a link and its click history. Consider update_link with archived: true instead if they might want it back.",
        input: DeleteLinkInput,
        output: DeleteLinkOut,
        // Waits for the user's approval on the confirm page (app.mcp.confirmRoute),
        // which shows describeDeleteLink's summary.
        destructive: { describe: describeDeleteLink },
        scope: "links:write",
      },
    },
  }),
];
