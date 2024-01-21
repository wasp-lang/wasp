export function config() {
  console.log('Inside user-supplied GitHub config')
  return {
    clientID: process.env['GITHUB_CLIENT_ID'],
    clientSecret: process.env['GITHUB_CLIENT_SECRET'],
    scope: [],
  }
}

export const userSignupFields = {}
