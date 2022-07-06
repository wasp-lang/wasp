async function onSignInFn(_method, context, args) {
    // TODO: Generate a secure, random password via a helper.
    const user = await findOrCreateUserEntity(context, args.profile.email, "password123!")
    return user
}

// TODO: Move into shared place for users.
async function findOrCreateUserEntity(context, email, password) {
  // TODO: Remove race condition.
  let user = await context.entities.User.findUnique({ where: { email } })
  if (!user) {
    user = await context.entities.User.create({ data: { email, password } })
  }
  return user
}

export default onSignInFn
