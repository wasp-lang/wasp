---
title: "Sending emails in Wasp: providers and setup"
description: Send emails from a Wasp app with emailSender. Choose the Dummy, SMTP, Mailgun, SendGrid or Resend provider, set its env vars in .env.server and call send. Read Dummy emails in the dev mailbox.
sidebar_label: Email
---

import { CardLink } from '@site/src/components/CardLink'

# Sending emails

With Wasp's email-sending feature, you can easily integrate email functionality into your web application.

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"

export default app({
  name: "myApp",
  emailSender: {
    provider: "<provider>",
    defaultFrom: {
      name: "Example",
      email: "hello@itsme.com",
    },
  },
  // ...
})
```

Choose from one of the providers:

- `Dummy` (development only),
- `Mailgun`,
- `SendGrid`,
- `Resend`
- or the good old `SMTP`.

Optionally, define the `defaultFrom` field, so you don't need to provide it whenever sending an email.

## Sending Emails

Before jumping into details about setting up various providers, let's see how easy it is to send emails.

You import the `emailSender` that is provided by the `wasp/server/email` module and call the `send` method on it.

```ts title="src/actions/sendEmail.ts" auto-js
import { emailSender } from "wasp/server/email";

// In some action handler...
const info = await emailSender.send({
  from: {
    name: "John Doe",
    email: "john@doe.com",
  },
  to: "user@domain.com",
  subject: "Saying hello",
  text: "Hello world",
  html: "Hello <strong>world</strong>",
});
```

Read more about the `send` method in the [API Reference](#javascript-api).

The `send` method returns an object with the status of the sent email. It varies depending on the provider you use.

## Providers

We'll go over all of the available providers in the next section. For some of them, you'll need to set up some env variables. You can do that in the `.env.server` file.

### Using the Dummy Provider {#dummy}

:::note Dummy Provider is not for production use

The `Dummy` provider is not for production use. It is only meant to be used during development. If you try building your app with the `Dummy` provider, the build will fail.
:::

To speed up development, Wasp offers a `Dummy` email sender. It doesn't send emails for real, so it doesn't require any setup. Instead, it logs the emails in the console and keeps them in the [dev mailbox](#dev-mailbox).

Set the provider to `Dummy` in your `main.wasp.ts` file.

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"

export default app({
  name: "myApp",
  emailSender: {
    provider: "Dummy",
  },
  // ...
})
```

Since Wasp sets `NODE_ENV` when it reads `main.wasp.ts`, you can use the `Dummy` provider in development and a real one in production:

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"

export default app({
  name: "myApp",
  emailSender: {
    provider: process.env.NODE_ENV === "production" ? "SMTP" : "Dummy",
  },
  // ...
})
```

#### Dev mailbox {#dev-mailbox}

The dev mailbox keeps the emails the `Dummy` provider sends, so you, your tests, and your AI agents can read them and follow their links (e.g., the email verification link).

You can reach the emails in several ways:

- **Inbox page**: open [http://localhost:3001/\_\_wasp/mailbox](http://localhost:3001/__wasp/mailbox) while `wasp start` is running. It updates live as emails arrive. The auth forms link to it too.
- **HTTP API**: see [the API](#dev-mailbox-api) below. It's handy for end-to-end tests and AI agents.
- **Files**: each email is a JSON file in `.wasp/mailbox/`, which you can read even when the server isn't running.
- **Console**: every email logged in the `wasp start` console comes with links to its inbox page, its API URL, and its file.

The emails survive server restarts. Wasp keeps the latest 500 emails by default, which you can change with `maxMessages`:

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"

export default app({
  name: "myApp",
  emailSender: {
    provider: "Dummy",
    mailbox: {
      // `Infinity` keeps all emails, `0` keeps none.
      maxMessages: 1000,
    },
  },
  // ...
})
```

##### Dev mailbox API {#dev-mailbox-api}

All routes are under `http://localhost:3001/__wasp/mailbox/api`. Reads are `GET` requests, while actions are `POST` requests with a JSON body.

| Route | Description |
| --- | --- |
| `GET /messages` | Lists email summaries, newest first. Accepts the `search`, `searchIn`, `after`, and `limit` query parameters. |
| `GET /messages/:id` | Returns one email. |
| `GET /messages/:id/html` | Returns the email's HTML body. |
| `GET /events` | Streams `added`, `deleted`, and `cleared` events ([server-sent events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events)). |
| `POST /messages.waitFor` | Returns the newest email matching `search`, `searchIn`, and `after`. If there is none yet, waits for one for up to `timeoutMs` milliseconds (default 10 seconds, at most 5 minutes) and responds with `408` if none arrives. |
| `POST /messages.delete` | Deletes the email with the given `id`. |
| `POST /messages.clear` | Deletes all emails. |

The filters work like this:

- `search` matches emails where any of the `searchIn` fields contains the text, ignoring case.
- `searchIn` lists the fields `search` looks in: `from`, `to`, `subject`, and `body`. It defaults to all of them. In query parameters, separate them with commas (`searchIn=subject,body`).
- `after` takes an email's `id` and matches only emails sent after it.
- `limit` caps how many emails you get back.

Each email has an `id`, `sentAt`, `from`, `to`, `subject`, `text`, `html`, and `links`. The `links` field lists the URLs found in the email, so you don't need to parse them yourself. Errors respond with a matching status code and an `{ error: { code, message } }` body.

For example, this is how you get the verification link after signing up `user@example.com`:

```sh
curl -X POST http://localhost:3001/__wasp/mailbox/api/messages.waitFor \
  -H "Content-Type: application/json" \
  -d '{ "search": "user@example.com", "searchIn": ["to"] }'
```

In a Playwright test, the same looks like this. `wasp/server/email/devMailbox` exports the API path and the message types, and it's safe to import from tests:

```ts title="e2e-tests/auth.spec.ts" auto-js
import { expect, test } from "@playwright/test"
import {
  devMailboxApiPath,
  type DevMailboxMessage,
} from "wasp/server/email/devMailbox"

test("can verify email", async ({ page, request }) => {
  const email = "user@example.com"
  // ... sign up as `email`

  const response = await request.post(
    `http://localhost:3001${devMailboxApiPath}/messages.waitFor`,
    { data: { search: email, searchIn: ["to"] } },
  )
  const { message }: { message: DevMailboxMessage } = await response.json()

  await page.goto(message.links[0])
  await expect(page.locator("body")).toContainText("Your email has been verified")
})
```

### Using the SMTP Provider {#smtp}

First, set the provider to `SMTP` in your `main.wasp.ts` file.

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"

export default app({
  name: "myApp",
  emailSender: {
    provider: "SMTP",
  },
  // ...
})
```

Then, add the following env variables to your `.env.server` file.

```properties title=".env.server"
SMTP_HOST=
SMTP_USERNAME=
SMTP_PASSWORD=
SMTP_PORT=
```

Many transactional email providers (e.g. Mailgun, SendGrid but also others) can also use SMTP, so you can use them as well.

:::caution SMTP ports might be blocked
Some hosting providers (for example, **Railway** on its free tier, or **Hetzner**) block outbound SMTP ports to prevent spam.
If you run into issues, check their documentation for a solution, or consider using a dedicated provider integration like [Mailgun](#mailgun) or [SendGrid](#sendgrid) instead of plain SMTP.
:::

### Using the Mailgun Provider {#mailgun}

Set the provider to `Mailgun` in the `main.wasp.ts` file.

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"

export default app({
  name: "myApp",
  emailSender: {
    provider: "Mailgun",
  },
  // ...
})
```

Then, get the Mailgun API key and domain and add them to your `.env.server` file.

#### Getting the API Key and Domain

1. Go to [Mailgun](https://www.mailgun.com/) and create an account.
2. Go to [Domains](https://app.mailgun.com/mg/sending/new-domain) and create a new domain.
3. Copy the domain and add it to your `.env.server` file.
4. Create a new Sending API key under `Send > Sending > Domain settings` and find `Sending API keys`.
5. Copy the API key and add it to your `.env.server` file.

```properties title=".env.server"
MAILGUN_API_KEY=
MAILGUN_DOMAIN=
```

#### Using the EU Region

If your domain region is in the EU, you need to set the `MAILGUN_API_URL` variable in your `.env.server` file:

```properties title=".env.server"
MAILGUN_API_URL=https://api.eu.mailgun.net
```

### Using the SendGrid Provider {#sendgrid}

:::caution SendGrid Free Plan Retired
As of May 27, 2025, SendGrid has [retired its free plans](https://www.twilio.com/en-us/changelog/sendgrid-free-plan). A paid SendGrid plan is now required to send emails. Consider using [Mailgun](#mailgun) or [SMTP](#smtp) with another provider if you need a free tier option.
:::

Set the provider field to `SendGrid` in your `main.wasp.ts` file.

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"

export default app({
  name: "myApp",
  emailSender: {
    provider: "SendGrid",
  },
  // ...
})
```

Then, get the SendGrid API key and add it to your `.env.server` file.

#### Getting the API Key

1. Go to [SendGrid](https://sendgrid.com/) and create an account (paid plan required).
2. Go to [API Keys](https://app.sendgrid.com/settings/api_keys) and create a new API key.
3. Copy the API key and add it to your `.env.server` file.

```properties title=".env.server"
SENDGRID_API_KEY=
```

### Using the Resend Provider {#resend}

Set the provider field to `Resend` in your `main.wasp.ts` file.

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"

export default app({
  name: "myApp",
  emailSender: {
    provider: "Resend",
  },
  // ...
})
```

Then, get the Resend API key and add it to your `.env.server` file.

#### Getting the API Key

1. Go to [Resend](https://resend.com/) and create an account.
2. Go to [API Keys](https://resend.com/api-keys) and create a new API key.
3. Copy the API key and add it to your `.env.server` file.

```properties title=".env.server"
RESEND_API_KEY=
```

## API Reference

### `emailSender` specification

<CardLink
  to="../api/@wasp/spec#emailsender-1"
  kind="api"
  title="EmailSender"
  description="All the options for the emailSender field of the app spec."
/>

### JavaScript API

<CardLink
  to="../api/wasp/server/email#emailsender-1"
  kind="api"
  title="emailSender"
  description="Send emails from your server code, and all the fields of an email."
/>
