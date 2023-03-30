import { v4 as uuidv4 } from 'uuid'

export function config() {
  console.log("Inside user-supplied GitHub config")
  return {
    clientID: process.env['GITHUB_CLIENT_ID'],
    clientSecret: process.env['GITHUB_CLIENT_SECRET'],
    scope: []
  }
}

export async function getUser(context, args) {
  console.log("Inside user-supplied GitHub getUser")
  console.log(args.profile)

  if (!args.profile.email) {
    throw new Error('No email associated with GitHub profile.')
  }

  // Find or create user based on GitHub email.
  const user = await context.entities.User.upsert({
    where: {
      email: args.profile.email,
    },
    update: {},
    create: {
      email: args.profile.email,
      password: uuidv4(),
    },
  })
  return user
}
