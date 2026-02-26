import { config } from "wasp/modules/config";
import { PACKAGE_NAME } from "./index.js";

type RuntimeConfig = {
  userEntityName: string;
  premiumPlanPriceId: string;
  subscriptionRoute: string;
  webhookRoute: string;
  successUrl: string;
  cancelUrl: string;
};

export const moduleConfig = config.get(PACKAGE_NAME)! as RuntimeConfig;
