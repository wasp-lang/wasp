import * as z from "zod";
import { createCommandWithCwd, runJsonCommand } from "../../../common/zx.js";
const RailwayApiServiceInstanceUpdateResponseSchema = z.object({
    data: z.object({
        serviceInstanceUpdate: z.literal(true),
    }),
});
export async function setServiceInstanceImage(serviceInstance, image, options) {
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
    await runServiceInstanceMutation(serviceInstance, serviceInstanceUpdateMutation, ["--variables", imageSourceInput], RailwayApiServiceInstanceUpdateResponseSchema, options);
}
const RailwayApiServiceInstanceDeployV2ResponseSchema = z.object({
    data: z.object({
        serviceInstanceDeployV2: z.string(),
    }),
});
export async function startServiceInstanceDeployment(serviceInstance, options) {
    const serviceInstanceDeployV2Mutation = `
    mutation ServiceInstanceDeployV2($serviceId: String!, $environmentId: String!) {
      serviceInstanceDeployV2(serviceId: $serviceId, environmentId: $environmentId)
    }
  `;
    await runServiceInstanceMutation(serviceInstance, serviceInstanceDeployV2Mutation, [], RailwayApiServiceInstanceDeployV2ResponseSchema, options);
}
async function runServiceInstanceMutation(serviceInstance, mutation, mutationArgs, responseSchema, options) {
    const railwayCli = createCommandWithCwd(options.railwayExe, options.waspProjectDir);
    return runJsonCommand(railwayCli, [
        "api",
        mutation,
        ...["--raw-var", `serviceId=${serviceInstance.serviceId}`],
        ...["--raw-var", `environmentId=${serviceInstance.environmentId}`],
        ...mutationArgs,
    ], responseSchema);
}
//# sourceMappingURL=serviceInstance.js.map