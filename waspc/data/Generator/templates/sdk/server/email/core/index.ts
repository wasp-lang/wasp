{{={= =}=}}
{=# isSmtpProviderUsed =}
// PRIVATE API
export { initSmtpEmailSender as initEmailSender } from "./providers/smtp.js";
{=/ isSmtpProviderUsed =}
{=# isSendGridProviderUsed =}
// PRIVATE API
export { initSendGridEmailSender as initEmailSender } from "./providers/sendgrid.js";
{=/ isSendGridProviderUsed =}
{=# isMailgunProviderUsed =}
// PRIVATE API
export { initMailgunEmailSender as initEmailSender } from "./providers/mailgun.js";
{=/ isMailgunProviderUsed =}
{=# isDummyProviderUsed =}
// PRIVATE API
export { initDummyEmailSender as initEmailSender } from "./providers/dummy.js";
{=/ isDummyProviderUsed =}
