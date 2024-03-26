import * as jwt from 'oslo/jwt'
import { config } from 'wasp/server'

const JWT_SECRET = new TextEncoder().encode(config.auth.jwtSecret)
const JWT_ALGORITHM = 'HS256'

// PRIVATE API
export function createJWT(
  data: Parameters<typeof jwt.createJWT>[2],
  options: Parameters<typeof jwt.createJWT>[3],
): Promise<string> {
  return jwt.createJWT(JWT_ALGORITHM, JWT_SECRET, data, options)
}

// PRIVATE API
export async function validateJWT<Payload>(token: string): Promise<Payload> {
  const { payload } = await jwt.validateJWT(JWT_ALGORITHM, JWT_SECRET, token)
  return payload as Payload
}

// PRIVATE API
export { TimeSpan } from 'oslo'
