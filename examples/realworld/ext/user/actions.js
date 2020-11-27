import { createNewUser } from '@wasp/core/auth.js'
import HttpError from '@wasp/core/HttpError.js'
import slug from 'slug'

export const signup = async ({ username, email, password }, context) => {
  try {
    await createNewUser({ username, email, password })
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
      // TODO: This is a problem because I save non-hashed password!!!!
      //   We somehow need to make it really hard (or impossible) for user to do this by mistake,
      //   because if even I did it by mistake, it is likely to happen again.
      //   I was used to mongoose doing hashing on save and was not aware it will just save it plain as day.
      //   Actually, is there even a mechanism I can use to change this password?
      //   I don't think so!
      password: newPassword || undefined
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
