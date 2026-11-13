// Input/output shapes for links. The same schemas validate Wasp operation
// args and MCP tool input.
import { cursorSchema, limitSchema, MAX_LIMIT } from "../shared/pagination";
import { z } from "../shared/zod";

export const SLUG_PATTERN = /^[a-z0-9-]{3,40}$/;

export const destinationUrlSchema = z
  .url({ protocol: /^https?$/, error: "Must be an http:// or https:// URL." })
  .max(2048)
  .describe(
    "The full URL the short link redirects to. Must start with http:// or https://.",
  );

export const slugSchema = z
  .string()
  .regex(
    SLUG_PATTERN,
    "Slugs are 3–40 characters: lowercase letters, numbers, and hyphens.",
  )
  .describe(
    "Custom short code used in the short URL (/r/<slug>). 3–40 chars of a-z, 0-9, and '-'.",
  );

export const titleSchema = z
  .string()
  .trim()
  .max(200)
  .describe("Optional human-readable label for the link.");

export const tagsSchema = z
  .array(z.string().trim().toLowerCase().min(1).max(30))
  .max(10)
  .describe(
    "Up to 10 tags (each up to 30 chars) for grouping and filtering links. Stored lowercase.",
  );

export const linkIdSchema = z
  .string()
  .min(1)
  .describe("The link's ID or its slug. Either works.");

export const LinkInput = z.object({
  destinationUrl: destinationUrlSchema,
  slug: slugSchema
    .optional()
    .describe("Custom slug. Omit it to get a random 7-character slug."),
  title: titleSchema
    .optional()
    .describe("Optional human-readable label for the link."),
  tags: tagsSchema
    .optional()
    .describe("Up to 10 tags (each up to 30 chars). Stored lowercase."),
});
export type LinkInput = z.infer<typeof LinkInput>;

export const LinkUpdate = z.object({
  destinationUrl: destinationUrlSchema
    .optional()
    .describe("New destination URL (http:// or https://)."),
  slug: slugSchema
    .optional()
    .describe("New slug. The old short URL stops working."),
  title: titleSchema
    .nullable()
    .optional()
    .describe("New title. Pass null to remove it."),
  tags: tagsSchema.optional().describe("Replaces the whole tag list."),
  archived: z
    .boolean()
    .optional()
    .describe(
      "Archived links stop redirecting (their short URL returns 404) and are hidden from lists by default.",
    ),
});
export type LinkUpdate = z.infer<typeof LinkUpdate>;

export const LinkListQuery = z.object({
  search: z
    .string()
    .trim()
    .max(200)
    .optional()
    .describe(
      "Case-insensitive text matched against slug, title, and destination URL.",
    ),
  tag: z
    .string()
    .trim()
    .toLowerCase()
    .max(30)
    .optional()
    .describe("Only links that have this tag."),
  archived: z
    .union([
      z.boolean(),
      z.enum(["true", "false"]).transform((v) => v === "true"),
    ])
    .default(false)
    .describe("false (default): only active links. true: only archived links."),
  cursor: cursorSchema,
  limit: limitSchema,
});
// What callers may pass (query strings arrive as strings).
export type LinkListQuery = {
  search?: string;
  tag?: string;
  archived?: boolean | "true" | "false";
  cursor?: string;
  limit?: number | string;
};

export const LinkOut = z.object({
  id: z.string().describe("Stable link ID."),
  slug: z.string().describe("Short code; the short URL is /r/<slug>."),
  destinationUrl: z.string().describe("Where the short link redirects."),
  title: z.string().nullable().describe("Optional label."),
  tags: z.array(z.string()).describe("Tags on the link."),
  archived: z
    .boolean()
    .describe("Whether the link is archived (not redirecting)."),
  createdAt: z.string().describe("ISO 8601 creation time."),
  updatedAt: z.string().describe("ISO 8601 last update time."),
  shortUrl: z.string().describe("The public short URL to share."),
  dashboardUrl: z.string().describe("Opens this link in the Linkly dashboard."),
});
export type LinkOut = z.infer<typeof LinkOut>;

export const LinkPage = z.object({
  data: z.array(LinkOut).describe("Links on this page, newest first."),
  nextCursor: z
    .string()
    .nullable()
    .describe(
      "Pass as `cursor` to get the next page; null when there are no more.",
    ),
});

// MCP tool inputs and outputs (expose.mcp in the *.wasp.ts files).
/** The `list_links` tool's input. */
export const ListLinksInput = z.object({
  search: z
    .string()
    .max(200)
    .optional()
    .describe(
      "Case-insensitive text matched against slug, title, and destination URL.",
    ),
  tag: z
    .string()
    .max(30)
    .optional()
    .describe("Only return links that have this tag."),
  archived: z
    .boolean()
    .optional()
    .describe(
      "false or omitted: active links only. true: archived links only.",
    ),
  cursor: z
    .string()
    .optional()
    .describe(
      "The `nextCursor` from a previous list_links call, to get the next page.",
    ),
  limit: z
    .number()
    .int()
    .min(1)
    .max(MAX_LIMIT)
    .optional()
    .describe(`How many links to return, 1–${MAX_LIMIT} (default 25).`),
});

/** The `get_link` tool's input. */
export const GetLinkInput = z.object({ id: linkIdSchema });

/** The `create_link` tool's input. */
export const CreateLinkInput = z.object({
  destinationUrl: destinationUrlSchema,
  slug: slugSchema
    .optional()
    .describe(
      "Custom slug (3–40 chars of a-z, 0-9, '-'). Omit it for a random one.",
    ),
  title: titleSchema
    .optional()
    .describe("Optional human-readable label for the link."),
  tags: tagsSchema
    .optional()
    .describe(
      "Up to 10 tags (each up to 30 chars) for grouping and filtering. Stored lowercase.",
    ),
});

/** The `update_link` tool's input. */
export const UpdateLinkInput = z.object({
  id: linkIdSchema,
  destinationUrl: destinationUrlSchema
    .optional()
    .describe("New destination URL (http:// or https://)."),
  slug: slugSchema
    .optional()
    .describe(
      "New slug (3–40 chars of a-z, 0-9, '-'). The old short URL stops working.",
    ),
  title: z
    .string()
    .max(200)
    .nullable()
    .optional()
    .describe("New title, or null to remove it."),
  tags: tagsSchema.optional().describe("Replaces the whole tag list."),
  archived: z
    .boolean()
    .optional()
    .describe("true: stop redirecting and hide from lists. false: restore."),
});

/** The `delete_link` tool's input. */
export const DeleteLinkInput = z.object({ id: linkIdSchema });

/** The `delete_link` tool's output. */
export const DeleteLinkOut = z.object({
  status: z.literal("deleted"),
  id: z.string().describe("ID of the deleted link."),
  slug: z.string().describe("Slug of the deleted link."),
});
