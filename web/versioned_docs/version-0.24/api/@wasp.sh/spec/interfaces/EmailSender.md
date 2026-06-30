# Interface: EmailSender

Email sender configuration.

Required for the email auth flows (verification, password reset) and
available for sending arbitrary emails via `wasp/server/email`.

See [Sending Emails](https://wasp.sh/docs/advanced/email).

## Example

```ts
import { app } from "@wasp.sh/spec"

export default app({
  // ...
  emailSender: {
    provider: "SMTP",
    defaultFrom: {
      name: "Example",
      email: "hello@itsme.com",
    },
  },
})
```

## Properties

### defaultFrom?

> `optional` **defaultFrom?**: [`EmailFromField`](EmailFromField.md)

The default sender's details. If you set this field, you don't need to
provide the `from` field when sending an email.

***

### provider

> **provider**: [`EmailSenderProviderName`](../type-aliases/EmailSenderProviderName.md)

Provider Wasp uses to deliver outgoing emails.
