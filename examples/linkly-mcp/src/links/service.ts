// All link business logic and authorization. Adapters (operations.ts and
// mcp.ts) only parse input, build a Principal, and call these.
import type { Link, Prisma, PrismaClient } from "@prisma/client";
import { randomInt } from "node:crypto";
import { dashboardUrls } from "../deeplinks/urls";
import { DomainError } from "../shared/errors";
import { decodeCursor, toPage, type Page } from "../shared/pagination";
import { requireScope, type Principal } from "../shared/principal";
import { shortUrl } from "../shared/urls";
import { parseInput } from "../shared/validate";
import {
  LinkInput,
  LinkListQuery,
  LinkUpdate,
  linkIdSchema,
  type LinkOut,
} from "./schemas";

export type Deps = { prisma: PrismaClient };

export function toLinkOut(link: Link): LinkOut {
  return {
    id: link.id,
    slug: link.slug,
    destinationUrl: link.destinationUrl,
    title: link.title,
    tags: link.tags,
    archived: link.archived,
    createdAt: link.createdAt.toISOString(),
    updatedAt: link.updatedAt.toISOString(),
    shortUrl: shortUrl(link.slug),
    dashboardUrl: dashboardUrls.link(link.id),
  };
}

export function linkNotFound(idOrSlug: string): DomainError {
  return new DomainError(
    "not_found",
    `Link "${idOrSlug}" not found. Use list_links to find valid IDs or slugs.`,
  );
}

/**
 * Looks up a link by ID or slug, scoped to the principal's user. Other users'
 * links are reported as not found, never forbidden, so IDs don't leak.
 */
export async function findOwnedLink(
  prisma: PrismaClient,
  userId: string,
  idOrSlug: string,
): Promise<Link> {
  const link = await prisma.link.findFirst({
    where: { userId, OR: [{ id: idOrSlug }, { slug: idOrSlug.toLowerCase() }] },
  });
  if (!link) throw linkNotFound(idOrSlug);
  return link;
}

export async function listLinks(
  p: Principal,
  query: LinkListQuery,
  { prisma }: Deps,
): Promise<Page<LinkOut>> {
  requireScope(p, "links:read");
  const q = parseInput(LinkListQuery, query);

  const where: Prisma.LinkWhereInput = {
    userId: p.userId,
    archived: q.archived,
  };
  if (q.tag) where.tags = { has: q.tag };
  if (q.search) {
    where.OR = [
      { slug: { contains: q.search, mode: "insensitive" } },
      { title: { contains: q.search, mode: "insensitive" } },
      { destinationUrl: { contains: q.search, mode: "insensitive" } },
    ];
  }
  if (q.cursor) {
    const c = decodeCursor(q.cursor);
    if (!c)
      throw new DomainError(
        "invalid",
        "Invalid cursor. Pass the `nextCursor` value from a previous response.",
      );
    where.AND = [
      {
        OR: [
          { createdAt: { lt: c.createdAt } },
          { createdAt: c.createdAt, id: { lt: c.id } },
        ],
      },
    ];
  }

  const rows = await prisma.link.findMany({
    where,
    orderBy: [{ createdAt: "desc" }, { id: "desc" }],
    take: q.limit + 1,
  });
  return toPage(rows, q.limit, toLinkOut);
}

export async function getLink(
  p: Principal,
  input: { id: string },
  { prisma }: Deps,
): Promise<LinkOut> {
  requireScope(p, "links:read");
  const id = parseInput(linkIdSchema, input.id);
  return toLinkOut(await findOwnedLink(prisma, p.userId, id));
}

export async function createLink(
  p: Principal,
  input: LinkInput,
  { prisma }: Deps,
): Promise<LinkOut> {
  requireScope(p, "links:write");
  const data = parseInput(LinkInput, input);
  const tags = uniq(data.tags ?? []);
  const title = data.title || null;

  if (data.slug) {
    await assertSlugFree(prisma, data.slug);
    try {
      return toLinkOut(
        await prisma.link.create({
          data: {
            userId: p.userId,
            slug: data.slug,
            destinationUrl: data.destinationUrl,
            title,
            tags,
          },
        }),
      );
    } catch (e) {
      throw asSlugConflict(e, data.slug);
    }
  }

  // Random slugs: retry on the (rare) collision.
  for (let attempt = 0; attempt < 5; attempt++) {
    const slug = randomSlug();
    try {
      return toLinkOut(
        await prisma.link.create({
          data: {
            userId: p.userId,
            slug,
            destinationUrl: data.destinationUrl,
            title,
            tags,
          },
        }),
      );
    } catch (e) {
      if (!isUniqueViolation(e)) throw e;
    }
  }
  throw new Error("Could not generate a unique slug.");
}

export async function updateLink(
  p: Principal,
  input: { id: string } & LinkUpdate,
  { prisma }: Deps,
): Promise<LinkOut> {
  requireScope(p, "links:write");
  const { id, ...patch } = input;
  const data = parseInput(LinkUpdate, patch);
  const link = await findOwnedLink(
    prisma,
    p.userId,
    parseInput(linkIdSchema, id),
  );

  if (data.slug && data.slug !== link.slug)
    await assertSlugFree(prisma, data.slug);
  try {
    const updated = await prisma.link.update({
      where: { id: link.id },
      data: {
        destinationUrl: data.destinationUrl,
        slug: data.slug,
        title: data.title === undefined ? undefined : data.title || null,
        tags: data.tags === undefined ? undefined : uniq(data.tags),
        archived: data.archived,
      },
    });
    return toLinkOut(updated);
  } catch (e) {
    throw data.slug ? asSlugConflict(e, data.slug) : e;
  }
}

/** Hard delete. MCP clients see delete_link marked destructive and may ask the user first. */
export async function deleteLink(
  p: Principal,
  input: { id: string },
  { prisma }: Deps,
): Promise<{ status: "deleted"; id: string; slug: string }> {
  requireScope(p, "links:write");
  const link = await findOwnedLink(
    prisma,
    p.userId,
    parseInput(linkIdSchema, input.id),
  );
  await prisma.link.delete({ where: { id: link.id } });
  return { status: "deleted", id: link.id, slug: link.slug };
}

const SLUG_ALPHABET = "abcdefghijkmnpqrstuvwxyz23456789"; // no look-alikes (l, o, 0, 1)

function randomSlug(length = 7): string {
  let slug = "";
  for (let i = 0; i < length; i++)
    slug += SLUG_ALPHABET[randomInt(SLUG_ALPHABET.length)];
  return slug;
}

async function assertSlugFree(
  prisma: PrismaClient,
  slug: string,
): Promise<void> {
  const taken = await prisma.link.findUnique({
    where: { slug },
    select: { id: true },
  });
  if (taken) throw slugConflict(slug);
}

function slugConflict(slug: string): DomainError {
  return new DomainError(
    "conflict",
    `The slug "${slug}" is already taken. Pick another slug, or omit it to get a random one.`,
  );
}

function asSlugConflict(e: unknown, slug: string): unknown {
  return isUniqueViolation(e) ? slugConflict(slug) : e;
}

function isUniqueViolation(e: unknown): boolean {
  return (
    typeof e === "object" &&
    e !== null &&
    (e as { code?: string }).code === "P2002"
  );
}

function uniq(values: string[]): string[] {
  return [...new Set(values)];
}
