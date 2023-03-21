---
title: Sending Emails
---

# Sending Emails

With Wasp's email sending feature, you can easily integrate email functionality into your web application.

```js title="main.wasp"
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

Choose from one of the providers - `Mailgun`, `SendGrid` or the good old `SMTP`. Optionally, define the `defaultFrom` field, so you don't need to provide it when sending an e-mail.

For each provider, you'll need to setup env variables in the `.env.server` file at the root of your project.

## Using the SMTP provider

First, set the provider to `SMTP` in your `main.wasp` file.

```js title="main.wasp"
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

## Using the Mailgun provider

First, we to set the provider to `Mailgun` in the `main.wasp` file.

```js title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: Mailgun,
  }
}
```

Next, we'll need the Mailgun API key and domain.

```properties title=".env.server"
MAILGUN_API_KEY=
MAILGUN_DOMAIN=
```

### Getting the API key and domain

1. Go to [Mailgun](https://www.mailgun.com/) and create an account.
2. Go to [API Keys](https://app.mailgun.com/app/account/security/api_keys) and create a new API key.
3. Copy the API key and add it to your `.env.server` file.
4. Go to [Domains](https://app.mailgun.com/app/domains) and create a new domain.
5. Copy the domain and add it to your `.env.server` file.

## Using the SendGrid provider

Let's start with adding the `emailSender` field to our `main.wasp` file.

```js title="main.wasp"
app Example {
  ...
  emailSender: {
    provider: SendGrid,
  }
}
```

Then we need to get a SendGrid API key and add it to our `.env.server` file.

```properties title=".env.server"
SENDGRID_API_KEY=
```

### Getting the API key

1. Go to [SendGrid](https://sendgrid.com/) and create an account.
2. Go to [API Keys](https://app.sendgrid.com/settings/api_keys) and create a new API key.
3. Copy the API key and add it to your `.env.server` file.
