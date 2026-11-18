import { Tag } from "wasp/entities";
import { HttpError } from "wasp/server";
import { type GetTags } from "wasp/server/operations";

export const getTags: GetTags<void, Tag[]> = (_, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  return context.entities.Tag.findMany({
    where: { user: { id: context.user.id } },
    orderBy: { name: "asc" },
  });
};
