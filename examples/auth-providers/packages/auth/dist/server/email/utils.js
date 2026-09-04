import { namespaceFor } from "../namespaces.js";
import { TimeSpan, makeJwt } from "../utils.js";
export function makeEmailHelpers(runtime) {
    const { createJWT } = makeJwt(runtime);
    async function createEmailJWT(email) {
        return createJWT({ email }, { expiresIn: new TimeSpan(30, "m") });
    }
    async function createEmailVerificationLink(email, clientRoute) {
        return `${runtime.clientUrl}${clientRoute}?token=${await createEmailJWT(email)}`;
    }
    async function createPasswordResetLink(email, clientRoute) {
        return `${runtime.clientUrl}${clientRoute}?token=${await createEmailJWT(email)}`;
    }
    async function sendEmailAndSaveMetadata(email, content, metadata) {
        // Save the metadata (e.g. timestamp) first, and then send the email so
        // the user can't send multiple requests while the email is being sent.
        const emailIdentities = runtime.identityNamespaces(namespaceFor("email"));
        const identity = await emailIdentities.find(email);
        if (!identity) {
            throw new Error(`User with email: ${email} not found.`);
        }
        await emailIdentities.updateData(email, metadata);
        // The `email-send` grant: present by construction, the email method
        // requires app.emailSender.
        if (runtime.email === undefined) {
            throw new Error("The email auth method requires the email-send grant.");
        }
        runtime.email.send(content).catch((e) => {
            console.error("Failed to send email", e);
        });
    }
    function sendPasswordResetEmail(email, content) {
        return sendEmailAndSaveMetadata(email, content, {
            passwordResetSentAt: new Date().toISOString(),
        });
    }
    function sendEmailVerificationEmail(email, content) {
        return sendEmailAndSaveMetadata(email, content, {
            emailVerificationSentAt: new Date().toISOString(),
        });
    }
    return {
        createEmailVerificationLink,
        createPasswordResetLink,
        sendPasswordResetEmail,
        sendEmailVerificationEmail,
    };
}
export function isEmailResendAllowed(fields, field, resendInterval = 1000 * 60) {
    const sentAt = fields[field];
    if (typeof sentAt !== "string" || !sentAt) {
        return { isResendAllowed: true, timeLeft: 0 };
    }
    const diff = Date.now() - new Date(sentAt).getTime();
    const isResendAllowed = diff > resendInterval;
    const timeLeft = isResendAllowed
        ? 0
        : Math.round((resendInterval - diff) / 1000);
    return { isResendAllowed, timeLeft };
}
