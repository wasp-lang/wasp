import type { PrismaClient } from "@prisma/client";

/** Active links only: missing and archived links both 404. */
export async function resolveRedirect(
  prisma: PrismaClient,
  slug: string,
): Promise<{ linkId: string; destinationUrl: string } | null> {
  const link = await prisma.link.findUnique({
    where: { slug: slug.toLowerCase() },
    select: { id: true, destinationUrl: true, archived: true },
  });
  if (!link || link.archived) return null;
  return { linkId: link.id, destinationUrl: link.destinationUrl };
}

export async function recordClick(
  prisma: PrismaClient,
  linkId: string,
  meta: { referer?: string; userAgent?: string },
): Promise<void> {
  await prisma.click.create({
    data: {
      linkId,
      referrer: referrerHost(meta.referer),
      userAgent: meta.userAgent?.slice(0, 200) || null,
    },
  });
}

/** Only the hostname is stored, never the full referring URL. */
export function referrerHost(referer: string | undefined): string | null {
  if (!referer) return null;
  try {
    return new URL(referer).hostname.toLowerCase() || null;
  } catch {
    return null;
  }
}
