import { findOrCreateUserEntity } from '@wasp/core/auth.js'

// NOTE: These functions are just samples for testing and same as the defaults.
export function config() {
  console.log("Inside user-supplied Google config")
  return {
    clientId: process.env['GOOGLE_CLIENT_ID'],
    clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
  }
}

export async function signInHandler(_context, args) {
  console.log("Inside user-supplied Google sign in function")
  const email = args.profile.emails[0].value
  return await findOrCreateUserEntity(email)
}
