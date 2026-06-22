---
title: Guided Tour
banner:
  content: |
    Have an Open SaaS app in production? <a href="https://e44cy1h4s0q.typeform.com/to/EPJCwsMi">We'll send you some swag! 👕</a>
---

Awesome, you now have your very own SaaS app up and running! But, first, here are some important things you need to know about your app in its current state:

1. When signing up with a new user, you will get a message to check your email for a verification link. But, in development, these emails are simply written to your terminal. **So, to continue with the registration process, check your server logs after sign up**! 
```sh title="server logs"
[ Server ] ╔═══════════════════════╗
[ Server ] ║ Dummy email sender ✉️  ║
[ Server ] ╚═══════════════════════╝
[ Server ] From:    Open SaaS App <me@example.com>
[ Server ] To:      vinny@wasp.sh
[ Server ] Subject: Verify your email
[ Server ] ═════════ Text ═════════
[ Server ] Click the link below to verify your email: http://localhost:3000/email-verification?token=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InZpbm55QHdhc3Auc2giLCJleHAiOjE3MTg5NjUyNTB9.PkRGrmuDPuYFXkTprf7QpAye0e_O9a70xbER6LfxGJw
[ Server ] ═════════ HTML ═════════
[ Server ] <p>Click the link below to verify your email</p>
[ Server ] <a href="http://localhost:3000/email-verification?token=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InZpbm55QHdhc3Auc2giLCJleHAiOjE3MTg5NjUyNTB9.PkRGrmuDPuYFXkTprf7QpAye0e_O9a70xbER6LfxGJw">Verify email</a> 
[ Server ] ════════════════════════
```
2. Your app is still missing some key configurations (e.g. API keys for Payment Processors, OpenAI, AWS S3, Auth, Analytics). These services won't work at the moment, but don't fear, because **we've provided detailed guides in these docs to help you set up all the services in this template**.
3. If you want to get a feel for what your SaaS could look like when finished, **check out [OpenSaaS.sh](https://opensaas.sh) in your browser. It was built using this template!** So make sure to log in, play around with the demo app, make a test payment, and check out the admin dashboard.

In the sections below, we will take a short guide through the codebase and the app's main features. Then at the end of this tour, we also prepared a checklist of likely changes you will want to make to the app to make it your own. 

We're looking forward to seeing what you build!

## Getting acquainted with the codebase
Now that you've gotten a first look at the app, let's dive into the codebase.

At the root of our project, you will see three folders:
```sh
.
├── app
├── blog
└── e2e-tests
```

`app` contains the Wasp project files, which is your full-stack React + NodeJS + Prisma app along with a Wasp config file, `main.wasp`, which will be explained in more detail below.

`blog` contains the [Astro Starlight template](https://starlight.astro.build/) for the blog and documentation section. 

`e2e-tests` contains the end-to-end tests using Playwright, which you can run to test your app's functionality.

### App File Structure

We've structured this full-stack app template vertically (by feature). That means that most directories within `app/src` contain both the React client code and NodeJS server code necessary for implementing its logic. 

Let's check out what's in the `app` folder in more detail:

:::caution[v0.13 and below]
If you are using an older version of the Open SaaS template with Wasp `v0.13.x` or below, you may see a slightly different file structure. But don't worry, the vast majority of the code and features are the same! 😅
:::

```sh
.
├── main.wasp              # Wasp Config file. You define your app structure here.
├── .wasp/                 # Output dir for Wasp. DON'T MODIFY THESE FILES!
├── public/                # Public assets dir, e.g. www.yourdomain.com/public-banner.webp
├── src/                   # Your code goes here.
│   ├── admin/             # Admin dashboard related pages and components.
│   ├── analytics/         # Logic and background jobs for processing analytics.
│   ├── auth/              # All auth-related pages/components and logic.
│   ├── client/            # Shared components, hooks, landing page, and other client code (React).
│   ├── demo-ai-app/       # Logic for the example AI-powered demo app.
│   ├── file-upload/       # Logic for uploading files to S3.
│   ├── landing-page       # Landing page related code
│   ├── messages           # Logic for app user messages.
│   ├── payment/           # Logic for handling payments and webhooks.
│   ├── server/            # Scripts, shared server utils, and other server-specific code (NodeJS).
│   ├── shared/            # Shared constants and util functions.
│   └── user/              # Logic related to users and their accounts.
├── .env.server            # Dev environment variables for your server code.
├── .env.client            # Dev environment variables for your client code.
├── .prettierrc            # Prettier configuration.
├── tailwind.config.js     # TailwindCSS configuration.
├── package.json
├── package-lock.json
└── .wasproot
```

### The Wasp Config file

This template at its core is a Wasp project, where [Wasp](https://wasp.sh) is a full-stack web app framework that let's you write your app in React, NodeJS, and Prisma and will manage the "boilerplatey" work for you, allowing you to just take care of the fun stuff!

[Wasp's secret sauce](https://wasp.sh/docs) is its use of a config file (`main.wasp`) and compiler which takes your code and outputs the client app, server app and deployment code for you. 

In this template, we've already defined a number of things in the `main.wasp` config file, including:

- [Auth](https://wasp.sh/docs/auth/overview)
- [Routes and Pages](https://wasp.sh/docs/tutorial/pages)
- [Prisma Database Models](https://wasp.sh/docs/data-model/entities)
- [Operations (data read and write functions)](https://wasp.sh/docs/data-model/operations/overview)
- [Background Jobs](https://wasp.sh/docs/advanced/jobs)
- [Email Sending](https://wasp.sh/docs/advanced/email)

By defining these things in the config file, Wasp continuously handles the boilerplate necessary with putting all these features together. You just need to focus on the business logic of your app.

Wasp abstracts away some things that you would normally be used to doing during development, so don't be surprised if you don't see some of the things you're used to seeing.

:::note
It's possible to learn Wasp's feature set simply through using this template, but if you find yourself unsure how to implement a Wasp-specific feature and/or just want to learn more, a great starting point is the intro tutorial in the [Wasp docs](https://wasp.sh/docs) which takes ~20 minutes.
:::

### Client

The `src/client` folder contains any additional client-side code that doesn't belong to a feature:

```sh
.
└── client
    ├── components         # Your shared React components.
    ├── fonts              # Extra fonts
    ├── hooks              # Your shared React hooks.
    ├── icons              # Your shared SVG icons.
    ├── static             # Assets that you need access to in your code, e.g. import logo from 'static/logo.png'
    ├── App.tsx            # Main app component to wrap all child components. Useful for global state, navbars, etc.
    ├── cn.ts              # Helper function for dynamic and conditional Tailwind CSS classes.
    └── Main.css

```

### Server

The `src/server` folder contains any additional server-side code that does not belong to a specific feature: 

```sh
└── server
    ├── scripts            # Scripts to run via Wasp, e.g. database seeding.
    └── utils.ts
```

## Main Features

### Auth

This template comes with a fully functional auth flow out of the box. It takes advantages of Wasp's built-in [Auth features](https://wasp.sh/docs/auth/overview), which do the dirty work of rolling your own full-stack auth for you!

```js title="main.wasp"
  auth: {
    userEntity: User,
    methods: {
      email: { 
        //...
      },
      google: {},
      github: {},
      discord: {}
    },
    onAuthFailedRedirectTo: "/",
  },
```

By defining the auth structure in your `main.wasp` file, Wasp manages all the necessary code for you, including:
- Email verified login with reset password
- Social login with Google and/or GitHub
- Auth-related database entities for user credentials, sessions, and social logins 
- Custom-generated AuthUI components for login, signup, and reset password
- Auth hooks for fetching user data

<!-- TODO: add pic of AuthUI components -->

We've set the template up with Wasp's `email`, `google`, and `gitHub` methods, which are all battle-tested and suitable for production. 

You can get started developing your app with the `email` method right away! 

:::caution[Dummy Email Provider]
Note that the `email` method relies on an `emailSender` (configured at `app.emailSender` in the `main.wasp` file), a service which sends emails to verify users and reset passwords. 

For development purposes, Wasp provides a `Dummy` email sender which Open SaaS comes with as the default. This provider *does not* actually send any confirmation emails to the specified email address, but instead logs all email verification links/tokens to the console! You can then follow these links to verify the user and continue with the sign-up process.

```tsx title="main.wasp" 
  emailSender: {
    provider: Dummy, // logs all email verification links/tokens to the server's console
    defaultFrom: {
      name: "Open SaaS App",
      email: "me@example.com" 
    },
  },
```
:::

We will explain more about these auth methods, and how to properly integrate them into your app, in the [Authentication Guide](/guides/authentication/).

### Subscription Payments with Stripe, Lemon Squeezy or Polar

No SaaS is complete without payments, specifically subscription payments. That's why this template comes with a fully functional Stripe, Lemon Squeezy and Polar integration. 

Let's take a quick look at how payments are handled in this template.

1. a user clicks the `BUY` button and a **Checkout session** is created on the server
2. the user is redirected to the Checkout page where they enter their payment info
3. the user is redirected back to the app and the Checkout session is completed
4. Stripe / Lemon Squeezy / Polar sends a webhook event to the server with the payment info
5. The app server's **webhook handler** handles the event and updates the user's subscription status

The payment processor you choose (Stripe, Lemon Squeezy or Polar) and its related functions can be found at `src/payment/paymentProcessor.ts`. The `PaymentProcessor` object holds the logic for creating checkout sessions, webhooks, etc.

The logic for creating the Checkout session is defined in the `src/payment/operation.ts` file. [Actions](https://wasp.sh/docs/data-model/operations/actions) are a type of Wasp Operation, specifically your server-side functions that are used to **write** or **update** data to the database. Once they're defined in the `main.wasp` file, you can easily call them on the client-side:

a) define the action in the `main.wasp` file
```js title="main.wasp"
action generateCheckoutSession {
  fn: import { generateCheckoutSession } from "@src/payment/operations",
  entities: [User]
}
```

b) implement the action in the `src/payment/operations` file
```js title="src/server/actions.ts"
export const generateCheckoutSession = async (paymentPlanId, context) => { 
  //...
 }
```

c) call the action on the client-side
```js title="src/client/app/SubscriptionPage.tsx"
import { generateCheckoutSession } from "wasp/client/operations";

const handleBuyClick = async (paymentPlanId) => {
  const checkoutSession = await generateCheckoutSession(paymentPlanId);
};
```

The webhook handler is defined in the `src/payment/webhook.ts` file. Unlike Actions and Queries in Wasp which are only to be used internally, we define the webhook handler in the `main.wasp` file as an API endpoint in order to expose it externally to the payment processor.

```js title="main.wasp"
api paymentsWebhook {
  fn: import { paymentsWebhook } from "@src/payment/webhook",
  httpRoute: (POST, "/payments-webhook") 
  entities: [User],
}
```

Within the webhook handler, we look for specific events that the Payment Processor sends us to let us know which payment was completed and for which user. Then we update the user's subscription status in the database.

To learn more about configuring the app to handle your products and payments, check out the [Payment Integrations guide](/guides/payment-integrations/).

:::tip[Star our Repo on GitHub! 🌟]
We've packed in a ton of features and love into this SaaS starter, and offer it all to you for free!

If you're finding this template and its guides useful, consider giving us [a star on GitHub](https://github.com/wasp-lang/wasp)
:::


### Analytics and Admin Dashboard

Keeping an eye on your metrics is crucial for any SaaS. That's why we've built an administrator's dashboard where you can view your app's stats, user data, and revenue all in one place.

<!-- TODO: add pic of admin dash -->

To do that, we've leveraged Wasp's [Jobs feature](https://wasp.sh/docs/advanced/jobs) to run a cron job that calculates your daily stats. The app stats, such as page views and sources, can be pulled from either Plausible or Google Analytics. All you have to do is create a project with the analytics provider of your choice and import the respective pre-built helper functions!

```js title="main.wasp"
job dailyStatsJob {
  executor: PgBoss,
  perform: {
    fn: import { calculateDailyStats } from "@src/analytics/stats"
  },
  schedule: {
    cron: "0 * * * *" // runs every hour
  },
  entities: [User, DailyStats, Logs, PageViewSource]
}
```

For more info on integrating Plausible or Google Analytics, check out the [Analytics guide](/guides/analytics/).

### SEO & Discoverability

Open SaaS ships SEO-friendly out of the box. Three pieces work together so your app shows up in search results and AI answers:

1. **Meta tags** in `main.wasp`'s `head` (title, description, Open Graph, Twitter cards) get injected into your landing page HTML.
2. **JSON-LD structured data (Schema Markup)** in `src/landing-page/components/SchemaMarkup.tsx` tells search engines and LLMs what your app is, who makes it, and what it offers — feeding rich snippets and AI-generated answers.
3. **Prerendering** turns your React marketing pages (e.g. landing page, pricing, FAQ, etc.) into static HTML at build time, so crawlers see your content (including the schema markup) without needing to execute JavaScript.

Customize the meta tags in `main.wasp` and the schema object in `SchemaMarkup.tsx` to match your product. See the [SEO guide](/guides/seo-performance/) for details.

## App Customization Walkthrough

### General Considerations

When you first start your Open SaaS app straight from the template, it will run, but many of the services won't work because they lack your own API keys. Here are list of services that need your API keys to work properly:

- Auth Methods (Google, GitHub)
- Stripe, Lemon Squeezy or Polar
- OpenAI (Chat GPT API)
- Email Sending (Sendgrid) -- you must set this up if you're using the `email` Auth method 
- Analytics (Plausible or Google Analytics)
- File Uploading (AWS S3)

Now would be a good time to decide which features you do and do not need for your app, and remove the ones from the codebase that you don't need.

For the features you will use, the next section of the documentation, `Guides`, will walk you through how to set each one up!

:::note[Open SaaS is built on Wasp]
Remember, this template is built on the Wasp framework. If, at any time, these docs fail to provide enough information about a certain built-in feature, make sure to check out the [Wasp docs](https://wasp.sh/docs)!
:::

But before you start setting up the main features, let's walk through the customizations you will likely want to make to the template to make it your own.

### Customizations Checklist
#### `main.wasp` Config File
- [ ] Change the app name and title:
  ```ts title="main.wasp" {1, 6}
    app YourAppName { 
      wasp: {
        version: "^0.13.2"
      },

      title: "Your App Name",
  ```
  :::caution[Restart Your App]
  Upon changing the app name, new, empty development database will be assigned to your app. This means you'll need to rerun `wasp db start`, `wasp db migrate-dev` and `wasp start`.
  :::
- [ ] Update meta tags in `app.head` (even if you don't have a custom domain yet, put one you would like to have, as this won't affect development).
- [ ] Update `app.emailSender.defaultFrom.name` with the name of your app/company/whatever you want your users to see in their inbox, if you're using the `emailSender` feature and/or `email` Auth method.
- [ ] Remove any features you might not use or need:
  - [ ] Auth methods - `app.auth.methods`
    - [ ] If you're not using `email` Auth method, remove the routes/pages `RequestPasswordReset`, `PasswordReset`, and `EmailVerification`
  - [ ] Email Sending - `app.emailSender`, `job emailChecker`
  - [ ] Plausible analytics - `app.head`
  - [ ] File Uploading - `entity File`, `route FileUploadRoute`, `action createFile`, `query getAllFilesByUser`, `getDownloadFileSignedURL`
- [ ] Rename Entites and their properties, Routes/Pages, & Operations, if you wish.

#### Customizing the Look / Style of the App
- [ ] Update your favicon at `public/favicon.ico`.
- [ ] Update the banner image used when posting links to your site at `public/public-banner.webp`.
  - [ ] Update the URL for this banner at `og:image` and `twitter:image` in `app.head` of the `main.wasp` file.
- [ ] Update the JSON-LD structured data in `src/landing-page/components/SchemaMarkup.tsx` with your app's name, description, URL, and other metadata. This helps search engines and LLMs understand your app for rich results and AI answers.
- [ ] Make changes to your landing page, `landingPage.tsx`.
  - [ ] Customize the `navBar`, `features`, `testimonials`, and `faqs` in the `contentSections.ts` file.
  - [ ] Change/rename the `logo.webp` and main hero banner (`open-saas-banner.webp`) in the `static` folder.
- [ ] If you want to make changes to the global styles of the app, you can do so in `tailwind.config.cjs`. **Be aware that the current custom global styles defined already are mostly used in the app's Admin Dashboard!**

#### Customizing the Analytics & Admin Dashboard
- [ ] If you're using Plausible, update the `app.head` with your Plausible domain.
- [ ] Update the `calculateDailyStats` function in `src/server/workers/calculateDailyStats.ts` to pull the stats from the analytics provider you've chosen (Plausible or Google Analytics).
- [ ] Change the cron schedule in the `dailyStatsJob` in the `main.wasp` file to match how often you want your stats to be calculated.
- [ ] Update the `AdminDashboard` components to display the stats you do/don't want to see.

#### `.env.server` and `.env.client` Files
- [ ] After you've followed the `Guides` in the next section, you'll need to update the `.env.server` and `.env.client` files with your API keys and other environment variables for the services you've decided to use.
- [ ] Delete any redundant environment variables that you're not using, from the `.env.*` files as well as the `.env.*.example` files.

#### Other Customizations
- [ ] Make a new GitHub Repo for your app.
- [ ] Deploy your app to a hosting provider.
- [ ] Buy a domain name for your app and get it set up with your hosting provider.
- [ ] Read the `e2e-tests` README and get your end-to-end tests set up.
  - [ ] Change the tests to suit the changes you've made to your app
- [ ] Get the CI pipeline set up for your app (you can get started by using the Open SaaS development CI [example here](https://github.com/wasp-lang/open-saas/tree/main/.github/workflows))

## What's next?

In the following `Guides` sections, we'll walk you through getting those API keys and setting up the finer points of features such as Payments & Webhooks, Auth, Email Sending, Analytics, and more.
