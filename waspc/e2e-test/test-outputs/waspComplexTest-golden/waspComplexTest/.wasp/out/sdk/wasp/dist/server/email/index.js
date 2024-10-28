import { initEmailSender } from "./core/index.js";
// TODO: We need to validate all the env variables
// For now, we are letting the runtime throw if they are not provided
const emailProvider = {
    type: "sendgrid",
    apiKey: process.env.SENDGRID_API_KEY,
};
// PUBLIC API
export const emailSender = initEmailSender(emailProvider);
//# sourceMappingURL=index.js.map