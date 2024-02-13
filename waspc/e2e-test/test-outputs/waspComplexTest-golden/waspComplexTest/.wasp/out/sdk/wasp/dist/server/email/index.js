import { initEmailSender } from "./core/index.js";
const emailProvider = {
    type: "sendgrid",
    apiKey: process.env.SENDGRID_API_KEY,
};
// PUBLIC API
export const emailSender = initEmailSender(emailProvider);
//# sourceMappingURL=index.js.map