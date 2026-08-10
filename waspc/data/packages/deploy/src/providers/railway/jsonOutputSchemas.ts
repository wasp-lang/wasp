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

const GroupedServiceInstancesSchema = z.object({
  edges: z.array(
    z.object({
      node: z.object({
        serviceInstances: z.object({
          edges: z.array(
            z.object({
              node: z.object({
                serviceName: z.string(),
                latestDeployment: z
                  .object({
                    status: DeploymentStatusSchema,
                  })
                  .nullish(),
              }),
            }),
          ),
        }),
      }),
    }),
  ),
});

export const RailwayCliProjectStatusSchema = z.object({
  environments: GroupedServiceInstancesSchema,
});

export type RailwayCliProjectStatus = z.infer<
  typeof RailwayCliProjectStatusSchema
>;

export const RailwayCliDomainSchema = z.union([
  z.object({ domains: z.array(z.string()).min(1) }),
  // Railway CLI 4.51 returns a single domain.
  z
    .object({ domain: z.string() })
    .transform(({ domain }) => ({ domains: [domain] })),
]);

export type RailwayCliDomain = z.infer<typeof RailwayCliDomainSchema>;
