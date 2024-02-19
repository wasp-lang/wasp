import { Argon2id } from 'oslo/password'

const argon2id = new Argon2id()

// PRIVATE API
export const hashPassword = async (password: string): Promise<string> => {
  return argon2id.hash(password)
}

// PRIVATE API
export const verifyPassword = async (hashedPassword: string, password: string): Promise<void> => {
  const isValidPassword = await argon2id.verify(hashedPassword, password)
  if (!isValidPassword) {
    throw new Error('Invalid password.')
  }
}
