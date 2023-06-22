---
title: Sending Emails
---

import SendingEmailsInDevelopment from '../_sendingEmailsInDevelopment.md'

# Sending Emails

With Wasp's email-sending feature, you can easily integrate email functionality into your web application.

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

Choose from one of the providers:
- `Mailgun`,
- `SendGrid`
- or the good old `SMTP`. 

Optionally, define the `defaultFrom` field, so you don't need to provide it whenever sending an e-mail.

## Sending e-mails

<SendingEmailsInDevelopment />

Before jumping into details about setting up various providers, let's see how easy it is to send e-mails. 

You import the `emailSender` that is provided by the `@wasp/email` module and call the `send` method on it.

```ts title="src/actions/sendEmail.js"
import { emailSender } from '@wasp/email/index.js'

// In some action handler...
const info = await emailSender.send({
  from: {
    name: 'John Doe',
    email: 'john@doe.com',
  },
  to: 'user@domain.com',
  subject: 'Saying hello',
  text: 'Hello world',
  html: 'Hello <strong>world</strong>'
})
```

Let's see what the `send` method accepts:

- `from` - the sender's details.
  - `name` - the name of the sender
  - `email` - the e-mail address of the sender
  - If you set up `defaultFrom` field in the `main.wasp`, this field is optional.
- `to` - the recipient's e-mail address
- `subject` - the subject of the e-mail
- `text` - the text version of the e-mail
- `html` - the HTML version of the e-mail

The `send` method returns an object with the status of the sent e-mail. It varies depending on the provider you use.

## Providers

For each provider, you'll need to set up env variables in the `.env.server` file at the root of your project.

## Using the SMTP provider

First, set the provider to `SMTP` in your `main.wasp` file.

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: SMTP,
  }
}
```

Then, add the following env variables to your `.env.server` file.

```properties title=".env.server"
SMTP_HOST=
SMTP_USERNAME=
SMTP_PASSWORD=
SMTP_PORT=
```

Many transactional email providers (e.g. Mailgun, SendGrid but also others) can also use SMTP, so you can use them as well.

## Using the Mailgun provider

Set the provider to `Mailgun` in the `main.wasp` file.

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: Mailgun,
  }
}
```

Then, get the Mailgun API key and domain and add them to your `.env.server` file.

### Getting the API key and domain

1. Go to [Mailgun](https://www.mailgun.com/) and create an account.
2. Go to [API Keys](https://app.mailgun.com/app/account/security/api_keys) and create a new API key.
3. Copy the API key and add it to your `.env.server` file.
4. Go to [Domains](https://app.mailgun.com/app/domains) and create a new domain.
5. Copy the domain and add it to your `.env.server` file.

```properties title=".env.server"
MAILGUN_API_KEY=
MAILGUN_DOMAIN=
```

## Using the SendGrid provider

Set the provider field to `SendGrid` in your `main.wasp` file.

```wasp title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: SendGrid,
  }
}
```

Then, get the SendGrid API key and add it to your `.env.server` file.

### Getting the API key

1. Go to [SendGrid](https://sendgrid.com/) and create an account.
2. Go to [API Keys](https://app.sendgrid.com/settings/api_keys) and create a new API key.
3. Copy the API key and add it to your `.env.server` file.

```properties title=".env.server"
SENDGRID_API_KEY=
```
