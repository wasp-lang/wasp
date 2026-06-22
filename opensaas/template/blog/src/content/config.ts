import { docsSchema, i18nSchema } from "@astrojs/starlight/schema";
import { defineCollection, z } from "astro:content";
import { blogSchema } from "starlight-blog/schema";

export const collections = {
  docs: defineCollection({
    schema: docsSchema({
      extend: (context) => {
        const blogSchemaResult = blogSchema(context);
        return z.object({
          ...blogSchemaResult.shape,
          subtitle: z.string().optional(),
          hideBannerImage: z.boolean().optional(),
        });
      },
    }),
  }),
  i18n: defineCollection({ type: "data", schema: i18nSchema() }),
};
