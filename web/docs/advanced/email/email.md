---
title: Sending Emails
---

import { Required } from '@site/src/components/Tag'
import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers'
import DummyProviderNote from './_dummy-provider-note.md'

# Sending Emails

With Wasp's email-sending feature, you can easily integrate email functionality into your web application.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: <provider>,
    defaultFrom: {
      name: "Example",
      email: "hello@itsme.com"
    },
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: <provider>,
    defaultFrom: {
      name: "Example",
      email: "hello@itsme.com"
    },
  }
}
```

</TabItem>
</Tabs>

Choose from one of the providers:

- `Dummy` (development only),
- `Mailgun`,
- `SendGrid`
- or the good old `SMTP`.

Optionally, define the `defaultFrom` field, so you don't need to provide it whenever sending an email.

## Sending Emails

Before jumping into details about setting up various providers, let's see how easy it is to send emails.

You import the `emailSender` that is provided by the `wasp/server/email` module and call the `send` method on it.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/actions/sendEmail.js"
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

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/actions/sendEmail.ts"
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

</TabItem>
</Tabs>

Read more about the `send` method in the [API Reference](#javascript-api).

The `send` method returns an object with the status of the sent email. It varies depending on the provider you use.

## Providers

We'll go over all of the available providers in the next section. For some of them, you'll need to set up some env variables. You can do that in the `.env.server` file.

### Using the Dummy Provider

<DummyProviderNote />

To speed up development, Wasp offers a `Dummy` email sender that `console.log`s the emails in the console. Since it doesn't send emails for real, it doesn't require any setup.

Set the provider to `Dummy` in your `main.wasp` file.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: Dummy,
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: Dummy,
  }
}
```

</TabItem>
</Tabs>

### Using the SMTP Provider

First, set the provider to `SMTP` in your `main.wasp` file.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: SMTP,
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: SMTP,
  }
}
```

</TabItem>
</Tabs>

Then, add the following env variables to your `.env.server` file.

```properties title=".env.server"
SMTP_HOST=
SMTP_USERNAME=
SMTP_PASSWORD=
SMTP_PORT=
```

Many transactional email providers (e.g. Mailgun, SendGrid but also others) can also use SMTP, so you can use them as well.

### Using the Mailgun Provider

Set the provider to `Mailgun` in the `main.wasp` file.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: Mailgun,
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: Mailgun,
  }
}
```

</TabItem>
</Tabs>

Then, get the Mailgun API key and domain and add them to your `.env.server` file.

#### Getting the API Key and Domain

1. Go to [Mailgun](https://www.mailgun.com/) and create an account.
1. Go to [Domains](https://app.mailgun.com/mg/sending/new-domain) and create a new domain.
1. Copy the domain and add it to your `.env.server` file.
1. Create a new Sending API key under `Send > Sending > Domain settings` and find `Sending API keys`.
1. Copy the API key and add it to your `.env.server` file.

```properties title=".env.server"
MAILGUN_API_KEY=
MAILGUN_DOMAIN=
```

#### Using the EU Region

If your domain region is in the EU, you need to set the `MAILGUN_API_URL` variable in your `.env.server` file:

```properties title=".env.server"
MAILGUN_API_URL=https://api.eu.mailgun.net
```


### Using the SendGrid Provider

Set the provider field to `SendGrid` in your `main.wasp` file.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: SendGrid,
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: SendGrid,
  }
}
```

</TabItem>
</Tabs>

Then, get the SendGrid API key and add it to your `.env.server` file.

#### Getting the API Key

1. Go to [SendGrid](https://sendgrid.com/) and create an account.
2. Go to [API Keys](https://app.sendgrid.com/settings/api_keys) and create a new API key.
3. Copy the API key and add it to your `.env.server` file.

```properties title=".env.server"
SENDGRID_API_KEY=
```

## API Reference

### `emailSender` dict

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: <provider>,
    defaultFrom: {
      name: "Example",
      email: "hello@itsme.com"
    },
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: <provider>,
    defaultFrom: {
      name: "Example",
      email: "hello@itsme.com"
    },
  }
}
```

</TabItem>
</Tabs>

The `emailSender` dict has the following fields:

- `provider: Provider` <Required />

  The provider you want to use. Choose from `Dummy`, `SMTP`, `Mailgun` or `SendGrid`.

  <DummyProviderNote />

- `defaultFrom: dict`

  The default sender's details. If you set this field, you don't need to provide the `from` field when sending an email.

### JavaScript API

Using the `emailSender` in <ShowForTs>Typescript</ShowForTs><ShowForJs>JavaScript</ShowForJs>:
<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/actions/sendEmail.js"
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

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/actions/sendEmail.ts"
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

</TabItem>
</Tabs>

The `send` method accepts an object with the following fields:

- `from: object`

  The sender's details. If you set up `defaultFrom` field in the `emailSender` dict in Wasp file, this field is optional.

  - `name: string`

    The name of the sender.

  - `email: string`

    The email address of the sender.

- `to: string` <Required />

  The recipient's email address.

- `subject: string` <Required />

  The subject of the email.

- `text: string` <Required />

  The text version of the email.

- `html: string` <Required />

  The HTML version of the email
