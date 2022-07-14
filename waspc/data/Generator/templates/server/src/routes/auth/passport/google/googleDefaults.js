import { findOrCreateUserEntity } from '../../../../core/auth.js'

export function configFn() {
  return {
    clientId: process.env['GOOGLE_CLIENT_ID'],
    clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
  }
}

export async function onSignInFn(_context, args) {
  const email = args.profile.emails[0].value
  return await findOrCreateUserEntity(email)
}
