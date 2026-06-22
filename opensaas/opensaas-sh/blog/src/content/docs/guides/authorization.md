---
title: Authorization
banner:
  content: |
    Have an Open SaaS app in production? <a href="https://e44cy1h4s0q.typeform.com/to/EPJCwsMi">We'll send you some swag! 👕</a>
---

This guide will help you get started with authorization in your SaaS app. 

Authorization refers to what users can access in your app. This is useful for differentiating between users who have paid for different subscription tiers (e.g. "hobby" vs "pro"), or between users who have admin privileges and those who do not.

Authorization differs from [authentication](/guides/authentication/) in that authentication refers to the process of verifying that a user is who they say they are (e.g. logging in with a username and password).

To learn more about the different types of user permissions built into this SaaS template, including Stripe subscription tiers and statuses, check out the [User Overview Reference](/general/user-overview/).

Also, check out our [blog post](https://wasp.sh/blog/2022/11/29/permissions-in-web-apps) to learn more about authorization (access control) in web apps.

### Client-side Authorization

Open Saas starts with all users having access to the landing page (`/`), but only authenticated users having access to the rest of the app (e.g. to the `/demo-app`, or to the `/account`).

To control which pages require users to be authenticated to access them, you can set the `authRequired` property of the corresponding `page` definition in your `main.wasp` file:

```tsx title="main.wasp" {3}
route AccountRoute { path: "/account", to: AccountPage }
page AccountPage {
  authRequired: true,
  component: import Account from "@src/user/AccountPage"
}
```

This will automatically redirect users to the login page if they are not logged in while trying to access that page.

:::caution[Client-side authorization is just for the looks]
Users can manipulate the client code as they wish, meaning that client-side access control (authorization) serves the purpose of ergonomics/user experience, not the purpose of restricting access to sensitive data.
This means that authorization in the client code is a nice-to-have: it is here to make sure users don't get lost in the part of the app they can't work with because data is missing due to them not having access, not to actually restrict them from doing something.
Actually ensuring they don't have access to the data, that is on the server to ensure, via server-side logic that you will implement for authorization (access control).
:::

If you want more fine-grained control over what users can access, there are two Wasp-specific options:
1. When you define the `authRequired: true` property on the `page` definition, Wasp automatically passes the User object to the page component. Here you can check for certain user properties before authorizing access:

```tsx title="ExamplePage.tsx" "{ user }: { user: User }"
import { type User } from "wasp/entities";

export function Example({ user }: { user: User }) {

  if (user.subscriptionStatus === 'past_due') {
    return (<span>Your subscription is past due. Please update your payment information.</span>)
  }
  if (user.subscriptionStatus === 'cancel_at_period_end') {
    return (<span>Your susbscription will end on 01.01.2024</span>)
  }
  if (user.subscriptionStatus === 'active') {
    return (<span>Thanks so much for your support!</span>)
  }

}
```

2. Or you can take advantage of the `useAuth` hook and check for certain user properties before authorizing access to certain pages or components:

```tsx title="ExamplePage.tsx" {1, 4}
import { useAuth } from "wasp/client/auth";

export function ExampleHomePage() {
  const { data: user } = useAuth();

  return (
    <h1> Hi {user.email || 'there'} 👋 </h1>
  )
}
```

### Server-side Authorization

Authorization on the server-side is the core of your access control logic, and determines what users actually can or can't do (unlike client-side authorization logic which is there merely for UX).

You can authorize access to server-side operations by adding a check for a logged-in user on the `context.user` object which is passed to all operations in Wasp:

```tsx title="src/server/actions.ts" 
export const someServerAction: SomeServerAction<...> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401); // throw an error if user is not logged in
  }

  if (context.user.subscriptionStatus === 'past_due') {
    throw new HttpError(403, 'Your subscription is past due. Please update your payment information.');
  }
  //...
}
```


