import { CheckCircle } from "lucide-react";
import { useState } from "react";
import { useNavigate } from "react-router";
import { useAuth } from "wasp/client/auth";
import {
  generateCheckoutSession,
  getCustomerPortalUrl,
  useQuery,
} from "wasp/client/operations";
import { routes } from "wasp/client/router";
import { Alert, AlertDescription } from "../client/components/ui/alert";
import { Button } from "../client/components/ui/button";
import {
  Card,
  CardContent,
  CardFooter,
  CardTitle,
} from "../client/components/ui/card";
import { cn } from "../client/utils";
import {
  PaymentPlanId,
  paymentPlans,
  prettyPaymentPlanName,
  SubscriptionStatus,
} from "./plans";

const bestDealPaymentPlanId: PaymentPlanId = PaymentPlanId.Pro;

interface PaymentPlanCard {
  name: string;
  price: string;
  description: string;
  features: string[];
}

export const paymentPlanCards: Record<PaymentPlanId, PaymentPlanCard> = {
  [PaymentPlanId.Hobby]: {
    name: prettyPaymentPlanName(PaymentPlanId.Hobby),
    price: "$9.99",
    description: "All you need to get started",
    features: ["Limited monthly usage", "Basic support"],
  },
  [PaymentPlanId.Pro]: {
    name: prettyPaymentPlanName(PaymentPlanId.Pro),
    price: "$19.99",
    description: "Our most popular plan",
    features: ["Unlimited monthly usage", "Priority customer support"],
  },
  [PaymentPlanId.Credits10]: {
    name: prettyPaymentPlanName(PaymentPlanId.Credits10),
    price: "$9.99",
    description: "One-time purchase of 10 credits for your account",
    features: ["Use credits for e.g. OpenAI API calls", "No expiration date"],
  },
};

export function PricingPage() {
  const [isPaymentLoading, setIsPaymentLoading] = useState<boolean>(false);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);

  const { data: user } = useAuth();
  const isUserSubscribed =
    !!user &&
    !!user.subscriptionStatus &&
    user.subscriptionStatus !== SubscriptionStatus.Deleted;

  const {
    data: customerPortalUrl,
    isLoading: isCustomerPortalUrlLoading,
    error: customerPortalUrlError,
  } = useQuery(getCustomerPortalUrl, { enabled: isUserSubscribed });

  const navigate = useNavigate();

  async function handleBuyNowClick(paymentPlanId: PaymentPlanId) {
    if (!user) {
      navigate(routes.LoginRoute.to);
      return;
    }
    try {
      setIsPaymentLoading(true);

      const checkoutResults = await generateCheckoutSession(paymentPlanId);

      if (checkoutResults?.sessionUrl) {
        window.open(checkoutResults.sessionUrl, "_self");
      } else {
        throw new Error("Error generating checkout session URL");
      }
    } catch (error: unknown) {
      console.error(error);
      if (error instanceof Error) {
        setErrorMessage(error.message);
      } else {
        setErrorMessage("Error processing payment. Please try again later.");
      }
      setIsPaymentLoading(false); // We only set this to false here and not in the try block because we redirect to the checkout url within the same window
    }
  }

  const handleCustomerPortalClick = () => {
    if (!user) {
      navigate(routes.LoginRoute.to);
      return;
    }

    if (customerPortalUrlError) {
      setErrorMessage("Error fetching Customer Portal URL");
      return;
    }

    if (!customerPortalUrl) {
      setErrorMessage(`Customer Portal does not exist for user ${user.id}`);
      return;
    }

    window.open(customerPortalUrl, "_blank");
  };

  return (
    <div className="py-10 lg:mt-10">
      <div className="mx-auto max-w-7xl px-6 lg:px-8">
        <div id="pricing" className="mx-auto max-w-4xl text-center">
          <h2 className="text-foreground mt-2 text-4xl font-bold tracking-tight sm:text-5xl">
            Pick your <span className="text-primary">pricing</span>
          </h2>
        </div>
        <p className="text-muted-foreground mx-auto mt-6 max-w-2xl text-center text-lg leading-8">
          Choose between Stripe, LemonSqueezy or Polar as your payment provider.
          Just add your Product IDs! Try it out below with test credit card
          number <br />
          <span className="bg-muted text-muted-foreground rounded-md px-2 py-1 font-mono text-sm">
            4242 4242 4242 4242 4242
          </span>
        </p>
        {errorMessage && (
          <Alert variant="destructive" className="mt-8">
            <AlertDescription>{errorMessage}</AlertDescription>
          </Alert>
        )}
        <div className="isolate mx-auto mt-16 grid max-w-md grid-cols-1 gap-y-8 sm:mt-20 lg:mx-0 lg:max-w-none lg:grid-cols-3 lg:gap-x-8">
          {Object.values(PaymentPlanId).map((planId) => (
            <Card
              key={planId}
              className={cn(
                "relative flex grow flex-col justify-between overflow-hidden transition-all duration-300 hover:shadow-lg",
                {
                  "ring-primary bg-transparent! ring-2":
                    planId === bestDealPaymentPlanId,
                  "ring-border ring-1 lg:my-8":
                    planId !== bestDealPaymentPlanId,
                },
              )}
            >
              {planId === bestDealPaymentPlanId && (
                <div
                  className="absolute right-0 top-0 -z-10 h-full w-full transform-gpu blur-3xl"
                  aria-hidden="true"
                >
                  <div
                    className="from-primary/40 via-primary/20 to-primary/10 bg-linear-to-br absolute h-full w-full opacity-30"
                    style={{
                      clipPath: "circle(670% at 50% 50%)",
                    }}
                  />
                </div>
              )}
              <CardContent className="h-full justify-between p-8 xl:p-10">
                <div className="flex items-center justify-between gap-x-4">
                  <CardTitle
                    id={planId}
                    className="text-foreground text-lg font-semibold leading-8"
                  >
                    {paymentPlanCards[planId].name}
                  </CardTitle>
                </div>
                <p className="text-muted-foreground mt-4 text-sm leading-6">
                  {paymentPlanCards[planId].description}
                </p>
                <p className="mt-6 flex items-baseline gap-x-1">
                  <span className="text-foreground text-4xl font-bold tracking-tight">
                    {paymentPlanCards[planId].price}
                  </span>
                  <span className="text-muted-foreground text-sm font-semibold leading-6">
                    {paymentPlans[planId].effect.kind === "subscription" &&
                      "/month"}
                  </span>
                </p>
                <ul
                  role="list"
                  className="text-muted-foreground mt-8 space-y-3 text-sm leading-6"
                >
                  {paymentPlanCards[planId].features.map((feature) => (
                    <li key={feature} className="flex gap-x-3">
                      <CheckCircle
                        className="text-primary h-5 w-5 flex-none"
                        aria-hidden="true"
                      />
                      {feature}
                    </li>
                  ))}
                </ul>
              </CardContent>
              <CardFooter>
                {isUserSubscribed ? (
                  <Button
                    onClick={handleCustomerPortalClick}
                    disabled={isCustomerPortalUrlLoading}
                    aria-describedby="manage-subscription"
                    variant={
                      planId === bestDealPaymentPlanId ? "default" : "outline"
                    }
                    className="w-full"
                  >
                    Manage Subscription
                  </Button>
                ) : (
                  <Button
                    onClick={() => handleBuyNowClick(planId)}
                    aria-describedby={planId}
                    variant={
                      planId === bestDealPaymentPlanId ? "default" : "outline"
                    }
                    className="w-full"
                    disabled={isPaymentLoading}
                  >
                    {user ? "Buy plan" : "Log in to buy plan"}
                  </Button>
                )}
              </CardFooter>
            </Card>
          ))}
        </div>
      </div>
    </div>
  );
}
