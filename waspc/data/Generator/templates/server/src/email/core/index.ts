{{={= =}=}}
{=# isSmtpProviderUsed =}
export { initSmtpEmailSender as initEmailSender } from "./providers/smtp.js";
{=/ isSmtpProviderUsed =}
{=# isSendGridProviderUsed =}
export { initSendGridEmailSender as initEmailSender } from "./providers/sendgrid.js";
{=/ isSendGridProviderUsed =}
{=# isMailgunProviderUsed =}
export { initMailgunEmailSender as initEmailSender } from "./providers/mailgun.js";
{=/ isMailgunProviderUsed =}
