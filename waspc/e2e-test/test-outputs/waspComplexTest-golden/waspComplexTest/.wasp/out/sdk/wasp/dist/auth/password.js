import SecurePassword from 'secure-password';
const SP = new SecurePassword();
// PRIVATE API
export const hashPassword = async (password) => {
    const hashedPwdBuffer = await SP.hash(Buffer.from(password));
    return hashedPwdBuffer.toString("base64");
};
// PRIVATE API
export const verifyPassword = async (hashedPassword, password) => {
    const result = await SP.verify(Buffer.from(password), Buffer.from(hashedPassword, "base64"));
    if (result !== SecurePassword.VALID) {
        throw new Error('Invalid password.');
    }
};
//# sourceMappingURL=password.js.map