// NOTE: These functions are just samples for testing and same as the defaults.
export function config() {
  console.log("Inside user-supplied Google config")
  return {
    clientId: process.env['GOOGLE_CLIENT_ID'],
    clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
  }
}

// TODO
export function firstSignInConfig(_context, args) {
  throw 'TODO'
}
