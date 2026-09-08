import * as z from "zod";
import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { createCommandWithCwd, runJsonCommand } from "../../../common/zx.js";
import { RailwayCliExe } from "../brandedTypes.js";

// Railway's name for a service in a specific environment.
export type RailwayServiceInstance = {
  serviceId: string;
  environmentId: string;
};

const RailwayApiServiceInstanceUpdateResponseSchema = z.object({
  data: z.object({
    serviceInstanceUpdate: z.literal(true),
  }),
});

export async function setServiceInstanceImage(
  serviceInstance: RailwayServiceInstance,
  image: string,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<void> {
  const serviceInstanceUpdateMutation = `
    mutation ServiceInstanceUpdate(
      $serviceId: String!
      $environmentId: String
      $input: ServiceInstanceUpdateInput!
    ) {
      serviceInstanceUpdate(
        serviceId: $serviceId
        environmentId: $environmentId
        input: $input
      )
    }
  `;

  const imageSourceInput = JSON.stringify({ input: { source: { image } } });
  await runServiceInstanceMutation(
    serviceInstance,
    serviceInstanceUpdateMutation,
    ["--variables", imageSourceInput],
    RailwayApiServiceInstanceUpdateResponseSchema,
    options,
  );
}

const RailwayApiServiceInstanceDeployV2ResponseSchema = z.object({
  data: z.object({
    serviceInstanceDeployV2: z.string(),
  }),
});

export async function startServiceInstanceDeployment(
  serviceInstance: RailwayServiceInstance,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<void> {
  const serviceInstanceDeployV2Mutation = `
    mutation ServiceInstanceDeployV2($serviceId: String!, $environmentId: String!) {
      serviceInstanceDeployV2(serviceId: $serviceId, environmentId: $environmentId)
    }
  `;

  await runServiceInstanceMutation(
    serviceInstance,
    serviceInstanceDeployV2Mutation,
    [],
    RailwayApiServiceInstanceDeployV2ResponseSchema,
    options,
  );
}

async function runServiceInstanceMutation<Schema extends z.ZodType>(
  serviceInstance: RailwayServiceInstance,
  mutation: string,
  mutationArgs: string[],
  responseSchema: Schema,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<z.infer<Schema>> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );

  return runJsonCommand(
    railwayCli,
    [
      "api",
      mutation,
      ...["--raw-var", `serviceId=${serviceInstance.serviceId}`],
      ...["--raw-var", `environmentId=${serviceInstance.environmentId}`],
      ...mutationArgs,
    ],
    responseSchema,
  );
}
