import SecurePassword from 'secure-password'

const SP = new SecurePassword()

export const hashPassword = async (password: string): Promise<string> => {
  const hashedPwdBuffer = await SP.hash(Buffer.from(password))
  return hashedPwdBuffer.toString("base64")
}

export const verifyPassword = async (hashedPassword: string, password: string): Promise<void> => {
  const result = await SP.verify(Buffer.from(password), Buffer.from(hashedPassword, "base64"))
  if (result !== SecurePassword.VALID) {
    throw new Error('Invalid password.')
  }
}
