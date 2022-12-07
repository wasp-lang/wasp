import { generateAvailableUsername } from '@wasp/core/auth.js'

export function config() {
  console.log("Inside user-supplied GitHub config")
  return {
    clientID: process.env['GITHUB_CLIENT_ID'],
    clientSecret: process.env['GITHUB_CLIENT_SECRET'],
    scope: []
  }
}

export async function getUserFields(_context, args) {
  console.log("Inside user-supplied GitHub getUserFields")
  const username = await generateAvailableUsername([args.profile.username], { separator: '-' })
  return { username }
}
