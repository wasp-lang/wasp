import HttpError from '@wasp/core/HttpError.js'

export const signup = async ({ username, email, password }, context) => {
  try {
    console.log(username, email, password)
    return await context.entities.User.create({
      data: { username, email, password }
    })
  } catch (err) {
    // TODO: I wish I didn't have to do this, I would love this to be in some
    //   degree done automatically.
    if (err.code == 'P2002') {
      throw new HttpError(400, err.meta.target + " must be unique.")
    }
    throw err
  }
}

export const updateUser = async ({ email, username, bio, profilePictureUrl, newPassword }, context) => {
  if (!context.user) { throw new HttpError(403) }

  // TODO: Nicer error handling! Right now everything is returned as 500 while it could be instead
  //   useful error message about username being taken / not unique, and other validation errors.
  await context.entities.User.update({
    where: { id: context.user.id },
    data: {
      email,
      username,
      bio,
      profilePictureUrl,
      ...(newPassword ? { password: newPassword } : {})
    }
  })
}

export const followUser = async ({ username, follow }, context) => {
  if (!context.user) { throw new HttpError(403) }

  await context.entities.User.update({
    where: { username },
    data: {
      followedBy: {
        ...(follow === true  ? { connect:    { id: context.user.id } } :
            follow === false ? { disconnect: { id: context.user.id } } :
            {}
           )
      }
    }
  })
}
