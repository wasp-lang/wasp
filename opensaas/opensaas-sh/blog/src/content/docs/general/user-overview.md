---
title: User Overview
banner:
  content: |
    Have an Open SaaS app in production? <a href="https://e44cy1h4s0q.typeform.com/to/EPJCwsMi">We'll send you some swag! 👕</a>
---

This reference will help you understand how the User entity works in this template.
This includes the user roles, subscription plans and statuses, and how to authorize access to certain pages and components.

## User Entity

The `User` entity within your app is defined in the `schema.prisma` file:

```tsx title="schema.prisma" ins="User: {}"
model User {
  id                        Int             @id @default(autoincrement())
  email                     String?         @unique
  username                  String?         
  createdAt                 DateTime        @default(now())
  isAdmin                   Boolean         @default(false)
  paymentProcessorUserId    String?         @unique
  lemonSqueezyCustomerPortalUrl String?     // You can delete this if you're not using Lemon Squeezy as your payments processor.
  subscriptionPlan          String?
  subscriptionStatus        String?
  sendEmail                 Boolean         @default(false)
  datePaid                  DateTime?
  credits                   Int             @default(3)
  gptResponses              GptResponse[]
  contactFormMessages       ContactFormMessage[]
  tasks                     Task[]
  files                     File[] 
}
```

We store all pertinent information to the user, including identification, subscription, and payment processor information. Meanwhile, Wasp abstracts away all the Auth related entities dealing with `passwords`, `sessions`, and `socialLogins`, so you don't have to worry about these at all in your Prisma schema (if you want to learn more about this process, check out the [Wasp Auth Docs](https://wasp.sh/docs/auth/overview)).

## Stripe and Subscriptions

We use Stripe to handle all of our subscription payments. The `User` entity has a number of fields that are related to Stripe and their ability to access features behind the paywall:

```tsx title="schema.prisma" {4-10}
model User {
  id                        Int             @id @default(autoincrement())
  //...
  paymentProcessorUserId    String?         @unique
  subscriptionPlan          String?
  subscriptionStatus        String?
  datePaid                  DateTime?
  credits                   Int             @default(3)
  //...
}
```

- `paymentProcessorUserId`: The payment processor customer ID. This is created on checkout and used to identify the customer.
- `subscriptionPlan`: The subscription plan the user is on. This is set by the app and is used to determine what features the user has access to. By default, we have three plans: `hobby` and `pro` subscription plans, as well as a `credits10` one-time purchase plan.
- `subscriptionStatus`: The subscription status of the user. This is set by the payment processor and is used to determine whether the user has access to the app or not. By default, we have four statuses: `active`, `past_due`, `cancel_at_period_end`, and `deleted`.
- `credits` (optional): By default, a user is given 3 credits to trial your product before they have to pay. You can create a one-time purchase product in Stripe to allow users to purchase more credits if they run out, e.g. the `credits10` plan in the template.

### Subscription Statuses

In general, we determine if a user has paid for an initial subscription by checking if the `subscriptionStatus` field is set. This field is set by Stripe within your webhook handler and is used to signify more detailed information on the user's current status. By default, the template handles four statuses: `active`, `past_due`, `cancel_at_period_end`, and `deleted`.

- When `active` the user has paid for a subscription and has full access to the app. 

- When `cancel_at_period_end`, the user has canceled their subscription and has access to the app until the end of their billing period. 

- When `deleted`, the user has reached the end of their subscription period after canceling and no longer has access to the app.

- When `past_due`, the user's automatic subscription renewal payment was declined (e.g. their credit card expired). You can choose how to handle this status within your app. For example, you can send the user an email to update their payment information:
```tsx title="src/payment/stripe/webhook.ts" 
import { emailSender } from "wasp/server/email";
//...

if (subscription.status === 'past_due') {
  const updatedCustomer = await context.entities.User.update({
    where: {
      id: customer.id,
    },
    data: {
      subscriptionStatus: 'past_due',
    },
  });

  if (updatedCustomer.email) {
    await emailSender.send({
      to: updatedCustomer.email,
      subject: 'Your Payment is Past Due',
      text: 'Please update your payment information to continue using our service.',
      html: '...',
    });
  }
}
```

See the client-side [authorization section](/guides/authorization/) below for more info on how to handle these statuses within your app.

### Subscription Plans

The `subscriptionPlan` field is used to determine what features the user has access to. 

By default, we have three plans: `hobby` and `pro` subscription plans, as well as a `credits10` one-time purchase plan. 

You can add more plans by adding more products and price IDs to your Stripe product and updating environment variables in your `.env.server` file as well as the relevant code in your app.

See the [Payment Integrations Guide](/guides/payment-integrations/) for more info on how to do this.

## User Roles

At the moment, we have two user roles: `admin` and `user`. This is defined within the `isAdmin` field in the `User` entity:

```tsx title="schema.prisma" {7}
model User {
  id                        Int             @id @default(autoincrement())
  email                     String?         @unique
  username                  String?
  createdAt                 DateTime        @default(now())
  isAdmin                   Boolean         @default(false)
//...
}
```

As an Admin, a user has access to the Admin dashboard, along with the user table where they can view and search for users, and edit and update information manually if necessary.

:::tip[Admin Privileges]
If you'd like to give yourself and/or certain users admin privileges, follow the instructions in the [Admin Dashboard](/general/admin-dashboard/#permissions) section.
:::

As a general User, a user has access to the user-facing app that sits behind the login, but not the Admin dashboard. You can further restrict access to certain features within the app by following the [authorization guide](/guides/authorization/).
