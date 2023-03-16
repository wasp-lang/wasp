import { generateAvailableUsername } from '@wasp/core/auth.js'

export function config() {
  console.log('Inside user-supplied Google config')
  return {
    clientID: process.env['GOOGLE_CLIENT_ID'],
    clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
    scope: ['profile'],
  }
}

export async function getUserFields(_context, args) {
  console.log('Inside user-supplied Google getUserFields')
  const username = await generateAvailableUsername(
    args.profile.displayName.split(' '),
    { separator: '.' }
  )
  return { username }
}
