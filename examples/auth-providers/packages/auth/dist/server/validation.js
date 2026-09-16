import { isValidEmail } from "@wasp.sh/lib-auth";
import { HttpError } from "./http.js";
/** Identities are keyed by the normalized address: trimmed and lower-cased. */
export function normalizeEmail(email) {
    return email.trim().toLowerCase();
}
/** Identities are keyed by the normalized username: trimmed and lower-cased. */
export function normalizeUsername(username) {
    return username.trim().toLowerCase();
}
export const PASSWORD_FIELD = "password";
const USERNAME_FIELD = "username";
const EMAIL_FIELD = "email";
const TOKEN_FIELD = "token";
export function ensureValidEmail(args) {
    validate(args, [
        {
            validates: EMAIL_FIELD,
            message: "email must be present",
            validator: (email) => !!email,
        },
        {
            validates: EMAIL_FIELD,
            message: "email must be a valid email",
            validator: (email) => isValidEmail(email),
        },
    ]);
}
export function ensureValidUsername(args) {
    validate(args, [
        {
            validates: USERNAME_FIELD,
            message: "username must be present",
            validator: (username) => !!username,
        },
    ]);
}
export function ensurePasswordIsPresent(args) {
    validate(args, [
        {
            validates: PASSWORD_FIELD,
            message: "password must be present",
            validator: (password) => !!password,
        },
    ]);
}
export function ensureValidPassword(args) {
    validate(args, [
        {
            validates: PASSWORD_FIELD,
            message: "password must be at least 8 characters",
            validator: (password) => isMinLength(password, 8),
        },
        {
            validates: PASSWORD_FIELD,
            message: "password must contain a number",
            validator: (password) => containsNumber(password),
        },
    ]);
}
export function ensureTokenIsPresent(args) {
    validate(args, [
        {
            validates: TOKEN_FIELD,
            message: "token must be present",
            validator: (token) => !!token,
        },
    ]);
}
export function throwValidationError(message) {
    throw new HttpError(422, "Validation failed", { message });
}
function validate(args, validators) {
    for (const { validates, message, validator } of validators) {
        if (!validator(args[validates])) {
            throwValidationError(message);
        }
    }
}
function isMinLength(input, minLength) {
    return typeof input === "string" && input.length >= minLength;
}
function containsNumber(input) {
    return typeof input === "string" && /\d/.test(input);
}
