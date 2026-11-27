import { z } from "zod";

export const DEFAULT_LIMIT = 25;
export const MAX_LIMIT = 100;

export const limitSchema = z.coerce
  .number()
  .int()
  .min(1)
  .max(MAX_LIMIT)
  .default(DEFAULT_LIMIT)
  .describe(
    `How many items to return, 1–${MAX_LIMIT}. Defaults to ${DEFAULT_LIMIT}.`,
  );

export const cursorSchema = z
  .string()
  .min(1)
  .optional()
  .describe(
    "Opaque cursor from a previous response's `nextCursor`, to fetch the next page.",
  );

export type Page<T> = { data: T[]; nextCursor: string | null };

// Cursors encode (createdAt, id) so pagination is stable under inserts.
export function encodeCursor(item: { createdAt: Date; id: string }): string {
  return Buffer.from(`${item.createdAt.toISOString()}|${item.id}`).toString(
    "base64url",
  );
}

export function decodeCursor(
  cursor: string,
): { createdAt: Date; id: string } | null {
  try {
    const [iso, id] = Buffer.from(cursor, "base64url")
      .toString("utf8")
      .split("|");
    const createdAt = new Date(iso ?? "");
    if (!id || Number.isNaN(createdAt.getTime())) return null;
    return { createdAt, id };
  } catch {
    return null;
  }
}

/** Takes `limit + 1` rows (newest first) and turns them into a page. */
export function toPage<T extends { createdAt: Date; id: string }, O>(
  rows: T[],
  limit: number,
  map: (row: T) => O,
): Page<O> {
  const hasMore = rows.length > limit;
  const pageRows = hasMore ? rows.slice(0, limit) : rows;
  const last = pageRows[pageRows.length - 1];
  return {
    data: pageRows.map(map),
    nextCursor: hasMore && last ? encodeCursor(last) : null,
  };
}
