import { v4 as uuidv4 } from 'uuid'

export function config() {
  console.log('Inside user-supplied Google config')
  return {
    clientID: process.env['GOOGLE_CLIENT_ID'],
    clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
    scope: ['profile', 'email'],
  }
}

export async function getUser(context, args) {
  console.log('Inside user-supplied Google getUser')
  console.log(args.profile.emails)

  // TODO: When would they have multiple?
  const email = args.profile.emails[0]

  // Find or create user based on Google email.
  const user = await context.entities.User.upsert({
    where: {
      email: email.value,
    },
    update: {},
    create: {
      email: email.value,
      isEmailVerified: email.verified,
      password: uuidv4(),
    },
  })
  return user
}
