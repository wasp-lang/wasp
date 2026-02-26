# @waspello/stripe-payments

A Wasp module that adds Stripe subscription payments.

## Install

```sh
npm install @waspello/stripe-payments
```

## Prisma requirements

Your User entity must include these Stripe fields:

```prisma
model User {
  id                 Int       @id @default(autoincrement())
  email              String?   @unique
  stripeCustomerId   String?   @unique
  subscriptionStatus String?
  subscriptionPlan   String?
  datePaid           DateTime?
}
```

## Usage

```ts
import { createStripePaymentsModule } from "@waspello/stripe-payments";

app.use(
  createStripePaymentsModule({
    userEntityName: "User",
    premiumPlanPriceId: process.env.STRIPE_PREMIUM_PRICE_ID!,
    subscriptionRoute: "/subscription",
  }),
);
```

## Environment variables

```
STRIPE_API_KEY=sk_test_...
STRIPE_WEBHOOK_SECRET=whsec_...
STRIPE_PREMIUM_PRICE_ID=price_...
```

## PayButton

The module exports a `PayButton` component you can place anywhere:

```tsx
import PayButton from "@waspello/stripe-payments/PayButton";
```
