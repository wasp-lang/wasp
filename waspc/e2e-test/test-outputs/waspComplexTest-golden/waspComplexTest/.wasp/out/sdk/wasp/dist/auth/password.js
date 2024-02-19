import { Argon2id } from 'oslo/password';
const argon2id = new Argon2id();
// PRIVATE API
export const hashPassword = async (password) => {
    return argon2id.hash(password);
};
// PRIVATE API
export const verifyPassword = async (hashedPassword, password) => {
    const isValidPassword = await argon2id.verify(hashedPassword, password);
    if (!isValidPassword) {
        throw new Error('Invalid password.');
    }
};
//# sourceMappingURL=password.js.map