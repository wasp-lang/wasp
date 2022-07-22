import { upsertUserWithRandomPassword } from '../../../../core/auth.js'

// Default implementation if there is no `auth.methods.google.configFn`.
export function configFn() {
  const clientId = process.env['GOOGLE_CLIENT_ID']
  const clientSecret = process.env['GOOGLE_CLIENT_SECRET']

  if (!clientId) {
    throw new Error("Missing GOOGLE_CLIENT_ID environment variable.")
  }

  if (!clientSecret) {
    throw new Error("Missing GOOGLE_CLIENT_SECRET environment variable.")
  }

  return { clientId, clientSecret, scope: ['email', 'profile'] }
}

// Default implementation if there is no `auth.methods.google.onSignInFn`.
export async function onSignInFn(_context, args) {
  const email = args.profile.emails[0].value

  // NOTE: This function helps us handle users authenticating via an external auth provider (e.g. Google).
  // We have two security-related scenarios to consider:
  // 1) If the user is new and comes in via external auth first, we simply create
  // their account with a random password. They cannot use `emailAndPassword`
  // to log with this email in until we add password reset functionality.
  // 2) If a user already exists from `emailAndPassword`, we cannot be
  // sure it really belongs to them since we do not yet do email validation.
  // Therefore, we *also* reset the password when we find an existing user, just
  // in case it was someone else who created that account. We do not want them to
  // still have access.
  // Upsert with a random password solves for both of these cases.
  return await upsertUserWithRandomPassword(email)
}
