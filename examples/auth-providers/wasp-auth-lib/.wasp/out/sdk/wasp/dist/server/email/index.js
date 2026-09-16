import { initEmailSender } from "./core/index.js";
const emailProvider = {
    type: "dummy",
};
// PUBLIC API
export const emailSender = initEmailSender(emailProvider);
//# sourceMappingURL=index.js.map