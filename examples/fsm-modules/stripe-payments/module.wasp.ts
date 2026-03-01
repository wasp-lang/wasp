import { createStripePaymentsModule } from "./src/index.js";

export default createStripePaymentsModule({
  premiumPlanPriceId: "price_example",
  subscriptionRoute: "/subscription",
});
