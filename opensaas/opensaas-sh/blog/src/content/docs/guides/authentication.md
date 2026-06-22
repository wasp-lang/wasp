---
title: Authentication
banner:
  content: |
    Have an Open SaaS app in production? <a href="https://e44cy1h4s0q.typeform.com/to/EPJCwsMi">We'll send you some swag! 👕</a>
---

Setting up your app's authentication is easy with Wasp. In fact, it's already set up for you in the `main.wasp` file: 

```tsx title="main.wasp"
  auth: {
    userEntity: User,
    methods: {
      email: {}, 
      google: {},
      gitHub: {},
      discord: {}
    },
    onAuthFailedRedirectTo: "/",
  },
```

The great part is, by defining your auth config in the `main.wasp` file, Wasp manages most of the Auth process for you, including the auth-related database entities for user credentials and sessions, as well as auto-generated client components for your app on the fly (aka AuthUI -- you can see them in use in the `src/auth` folder).

## Email Verified Auth

`email` method is the default auth method in Open Saas.

Since it needs to send emails to verify users and reset passwords, it requires an [email sender](https://wasp.sh/docs/advanced/email) provider: a service it can use to send emails.
"email sender" provider is configured via `app.emailSender` field in the `main.wasp` file.

:::caution[Dummy Email Provider]
To make it easy for you to get started, Open SaaS initially comes with the `Dummy` "email sender" provider, which does not send any emails, but instead logs all email verification links/tokens to the server's console!
You can then follow these links to verify the user and continue with the sign-up process.

```tsx title="main.wasp"
  emailSender: {
    provider: Dummy, // logs all email verification links/tokens to the server's console
    defaultFrom: {
      name: "Open SaaS App",
      email: "me@example.com" 
    },
  },
```

You **can not use the Dummy provider in production** and your app **will not build** until you move to a production-ready provider, such as SendGrid. We outline the process of migrating to SendGrid below. 
:::

In order to use the `email` auth method in production, you'll need to switch from the `Dummy` "email sender" provider to a production-ready provider like SendGrid: 

1. First, set up your app's `emailSender` in the `main.wasp` file by following [this guide](/guides/email-sending/#integrate-your-email-sender). 
2. Add your `SENDGRID_API_KEY` to the `.env.server` file.
3. Make sure the email address you use in the `fromField` object is the same email address that you configured your SendGrid account to send out emails with. In the end, your `main.wasp` file should look something like this: 
```ts title="main.wasp" {6,7} del={15} ins={16}
  auth: {
    methods: {
      email: {
        fromField: {
          name: "Open SaaS App",
          // When using SendGrid, you must use the same email address that you configured your account to send out emails with!
          email: "me@example.com" 
        },
        //...
      }, 
    }
  },
  //...
  emailSender: {
    provider: Dummy,
    provider: SendGrid,
    defaultFrom: {
      name: "Open SaaS App",
      // When using SendGrid, you must use the same email address that you configured your account to send out emails with!
      email: "me@example.com" 
    },
  },
  ```


And that's it. Wasp will take care of the rest and update your AuthUI components accordingly.

Check out the  [Wasp Auth docs](https://wasp.sh/docs/auth/overview) for more info.

## Google, GitHub, & Discord Auth

We've also customized and pre-built the Google and GitHub auth flow for you. To start using them, you just need to uncomment out the methods you want in your `main.wasp` file and obtain the proper API keys to add to your `.env.server` file. 

To create a Google OAuth app and get your Google API keys, follow the instructions in [Wasp's Google Auth docs](https://wasp.sh/docs/auth/social-auth/google#3-creating-a-google-oauth-app).

To create a GitHub OAuth app and get your GitHub API keys, follow the instructions in [Wasp's GitHub Auth docs](https://wasp.sh/docs/auth/social-auth/github#3-creating-a-github-oauth-app).

To create a Discord OAuth app and get your Discord API keys, follow the instructions in [Wasp's Discord Auth docs](https://wasp.sh/docs/auth/social-auth/discord#3-creating-a-discord-app)

Again, Wasp will take care of the rest and update your AuthUI components accordingly.
