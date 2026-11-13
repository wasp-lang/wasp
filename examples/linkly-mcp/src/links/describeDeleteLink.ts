import { prisma } from "wasp/server";
import type { McpDescribeFn } from "wasp/server/mcp";

/** What the confirm page shows for delete_link: the link, not just its ID. */
export const describeDeleteLink: McpDescribeFn<{ id: string }> = async (
  { id },
  { user },
) => {
  const link = await prisma.link.findFirst({
    where: { id, userId: user.id },
    include: { _count: { select: { clicks: true } } },
  });
  if (!link) return { title: `Delete link ${id}` };
  return {
    title: `Delete the short link /${link.slug}`,
    details: `It points to ${link.destinationUrl}. Its ${link._count.clicks} recorded clicks are deleted too.`,
  };
};
