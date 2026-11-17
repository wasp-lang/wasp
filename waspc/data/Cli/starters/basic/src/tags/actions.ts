import { Tag } from "wasp/entities";
import { ServerHttpError } from "wasp/server";
import { CreateTag } from "wasp/server/operations";

type CreateTagArgs = Pick<Tag, "name" | "color">;

export const createTag: CreateTag<CreateTagArgs, Tag> = (tag, context) => {
  if (!context.user) {
    throw new ServerHttpError(401);
  }

  return context.entities.Tag.create({
    data: {
      name: tag.name,
      color: tag.color,
      user: {
        connect: {
          id: context.user.id,
        },
      },
    },
  });
};
