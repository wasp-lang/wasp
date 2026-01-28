import * as z from "zod";
import { canonicalSchema } from "../../common/schema";

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

export const RailwayCliDomainSchema = canonicalSchema(
  z.array(z.string()).nonempty(),
  [
    // Railway CLI >=4.18.1
    z.object({ domains: z.unknown() }).transform(({ domains }) => domains),

    // Railway CLI <4.18.1
    z.object({ domain: z.unknown() }).transform(({ domain }) => [domain]),
  ],
);

export type RailwayCliDomain = z.infer<typeof RailwayCliDomainSchema>;
