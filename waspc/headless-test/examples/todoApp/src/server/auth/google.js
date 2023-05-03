export function config() {
  console.log('Inside user-supplied Google config')
  return {
    clientID: process.env['GOOGLE_CLIENT_ID'],
    clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
    scope: ['profile', 'email'],
  }
}

export async function getUserFields(_context, args) {
  console.log('Inside user-supplied Google getUserFields')
  const email =
    args.profile.emails.length > 0 ? args.profile.emails[0].value : null
  return { email, isEmailVerified: !!email }
}
