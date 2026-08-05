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

// Railway CLI >=4.35 nests service instances under `environments`.
const NewFormatProjectStatusSchema = z.object({
  environments: GroupedServiceInstancesSchema,
});

// Older Railway CLI versions (e.g. 4.11) nest service instances under `services`.
const OldFormatProjectStatusSchema = z.object({
  services: GroupedServiceInstancesSchema,
});

export const RailwayCliProjectStatusSchema = z.union([
  NewFormatProjectStatusSchema,
  OldFormatProjectStatusSchema.transform(({ services }) => ({
    environments: services,
  })),
]);

export type RailwayCliProjectStatus = z.infer<
  typeof RailwayCliProjectStatusSchema
>;

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
