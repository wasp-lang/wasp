import { type EmailSender } from "@wasp.sh/spec";

export const emailSender: EmailSender = {
  // NOTE: "Dummy" provider is just for local development purposes.
  //   Make sure to check the server logs for the email confirmation url (it will not be sent to an address)!
  //   Once you are ready for production, switch to e.g. "SendGrid" or "Mailgun" providers. Check out https://docs.opensaas.sh/guides/email-sending/ .
  provider: "Dummy",
  defaultFrom: {
    name: "Open SaaS App",
    // When using a real provider, e.g. SendGrid, you must use the same email address that you configured your account to send out emails with!
    email: "me@example.com",
  },
};
