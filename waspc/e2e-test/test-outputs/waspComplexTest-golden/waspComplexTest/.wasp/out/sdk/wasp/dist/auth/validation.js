import { HttpError } from 'wasp/server';
export const PASSWORD_FIELD = 'password';
const USERNAME_FIELD = 'username';
const EMAIL_FIELD = 'email';
const TOKEN_FIELD = 'token';
// PUBLIC API
export function ensureValidEmail(args) {
    validate(args, [
        { validates: EMAIL_FIELD, message: 'email must be present', validator: email => !!email },
        { validates: EMAIL_FIELD, message: 'email must be a valid email', validator: email => isValidEmail(email) },
    ]);
}
// PUBLIC API
export function ensureValidUsername(args) {
    validate(args, [
        { validates: USERNAME_FIELD, message: 'username must be present', validator: username => !!username }
    ]);
}
// PUBLIC API
export function ensurePasswordIsPresent(args) {
    validate(args, [
        { validates: PASSWORD_FIELD, message: 'password must be present', validator: password => !!password },
    ]);
}
// PUBLIC API
export function ensureValidPassword(args) {
    validate(args, [
        { validates: PASSWORD_FIELD, message: 'password must be at least 8 characters', validator: password => isMinLength(password, 8) },
        { validates: PASSWORD_FIELD, message: 'password must contain a number', validator: password => containsNumber(password) },
    ]);
}
// PUBLIC API
export function ensureTokenIsPresent(args) {
    validate(args, [
        { validates: TOKEN_FIELD, message: 'token must be present', validator: token => !!token },
    ]);
}
// PRIVATE API
export function throwValidationError(message) {
    throw new HttpError(422, 'Validation failed', { message });
}
function validate(args, validators) {
    for (const { validates, message, validator } of validators) {
        if (!validator(args[validates])) {
            throwValidationError(message);
        }
    }
}
// NOTE(miho): it would be good to replace our custom validations with e.g. Zod
const validEmailRegex = /(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])/;
function isValidEmail(input) {
    if (typeof input !== 'string') {
        return false;
    }
    return input.match(validEmailRegex) !== null;
}
function isMinLength(input, minLength) {
    if (typeof input !== 'string') {
        return false;
    }
    return input.length >= minLength;
}
function containsNumber(input) {
    if (typeof input !== 'string') {
        return false;
    }
    return /\d/.test(input);
}
//# sourceMappingURL=validation.js.map