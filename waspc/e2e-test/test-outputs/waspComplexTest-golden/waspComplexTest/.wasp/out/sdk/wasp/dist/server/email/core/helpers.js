// PRIVATE API
// Formats an email address and an optional name into a string that can be used
// as the "from" field in an email.
// { email: "test@test.com, name: "Test" } -> "Test <test@test.com>"
export function formatFromField({ email, name, }) {
    if (name) {
        return `${name} <${email}>`;
    }
    return email;
}
// PRIVATE API
export function getDefaultFromField() {
    return {
        email: "hello@itsme.com",
        name: "Hello",
    };
}
//# sourceMappingURL=helpers.js.map