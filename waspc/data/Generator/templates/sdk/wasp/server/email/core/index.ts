{{={= =}=}}
{=# isSmtpProviderEnabled =}
// PRIVATE API
export { initSmtpEmailSender as initEmailSender } from "./providers/smtp.js";
{=/ isSmtpProviderEnabled =}
{=# isSendGridProviderEnabled =}
// PRIVATE API
export { initSendGridEmailSender as initEmailSender } from "./providers/sendgrid.js";
{=/ isSendGridProviderEnabled =}
{=# isMailgunProviderEnabled =}
// PRIVATE API
export { initMailgunEmailSender as initEmailSender } from "./providers/mailgun.js";
{=/ isMailgunProviderEnabled =}
{=# isResendProviderEnabled =}
// PRIVATE API
export { initResendEmailSender as initEmailSender } from "./providers/resend.js";
{=/ isResendProviderEnabled =}
{=# isDummyProviderEnabled =}
// PRIVATE API
export { initDummyEmailSender as initEmailSender } from "./providers/dummy.js";
{=/ isDummyProviderEnabled =}
