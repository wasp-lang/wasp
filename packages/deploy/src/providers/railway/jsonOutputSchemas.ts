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

export const RailwayCliEnvironmentListSchema = z.object({
  environments: z.array(
    z.object({
      id: z.string(),
      name: z.string(),
      isLinked: z.boolean(),
    }),
  ),
});

export const RailwayCliServiceListSchema = z.array(
  RailwayCliServiceSchema.extend({
    volumes: z.array(
      z.object({
        mountPath: z.string(),
      }),
    ),
  }),
);

export const DeploymentStatusSchema = z.enum([
  "BUILDING",
  "CRASHED",
  "DEPLOYING",
  "FAILED",
  "INITIALIZING",
  "NEEDS_APPROVAL",
  "QUEUED",
  "REMOVED",
  "REMOVING",
  "SKIPPED",
  "SLEEPING",
  "SUCCESS",
  "WAITING",
]);

export type DeploymentStatus = z.infer<typeof DeploymentStatusSchema>;

export const RailwayCliServiceStatusSchema = z.object({
  // A missing status (service never deployed) or an unrecognized status
  // defaults to `null`.
  status: DeploymentStatusSchema.nullable().catch(null).default(null),
});

export const RailwayCliDomainSchema = z.union([
  // `railway domain` prints all existing domains when the service already
  // has one...
  z.object({ domains: z.array(z.string()).min(1) }),
  // ...and just the new domain when it creates it.
  z
    .object({ domain: z.string() })
    .transform(({ domain }) => ({ domains: [domain] })),
]);

export type RailwayCliDomain = z.infer<typeof RailwayCliDomainSchema>;
