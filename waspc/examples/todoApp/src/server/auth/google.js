export function config() {
  console.log('Inside user-supplied Google config')
  return {
    clientID: process.env['GOOGLE_CLIENT_ID'],
    clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
    scope: ['profile', 'email'],
  }
}

export function getUserFields({ profile }) {
  console.log('Inside user-supplied Google getUserFields')
  return {}
}
