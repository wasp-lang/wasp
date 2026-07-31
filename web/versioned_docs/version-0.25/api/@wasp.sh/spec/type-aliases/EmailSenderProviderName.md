# Type Alias: EmailSenderProviderName

> **EmailSenderProviderName** = `"SMTP"` \| `"SendGrid"` \| `"Mailgun"` \| `"Resend"` \| `"Dummy"`

Supported email sender providers.

`"Dummy"` logs emails to the console instead of sending them, which is
convenient during development, but not allowed in production.
