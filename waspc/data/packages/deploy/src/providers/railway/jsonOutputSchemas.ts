import * as z from "zod";

export type RailwayCliService = z.infer<typeof RailwayCliServiceSchema>;

export const RailwayCliServiceSchema = z.object({
  id: z.string(),
  name: z.string(),
});

export type RailwayCliProject = z.infer<typeof RailwayCliProjectSchema>;

export const RailwayCliProjectSchema = z.object({
  id: z.string(),
  name: z.string(),
  services: z.object({
    edges: z.array(
      z.object({
        node: RailwayCliServiceSchema,
      }),
    ),
  }),
});

export const RailwayProjectListSchema = z.array(RailwayCliProjectSchema);

export const RailwayCliDomainSchema = z.union([
  // Railway CLI >=4.18.1
  z.object({ domains: z.array(z.string()).min(1) }),

  // Railway CLI <4.18.1
  z
    .object({ domain: z.string() })
    // Convert to the newer format
    .transform(({ domain }) => ({ domains: [domain] })),
]);

export type RailwayCliDomain = z.infer<typeof RailwayCliDomainSchema>;
