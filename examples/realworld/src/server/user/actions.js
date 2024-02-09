import HttpError from '@wasp/core/HttpError.js'
import { isPrismaError, prismaErrorToHttpError } from '@wasp/utils.js'

export const updateUser = async ({ email, username, bio, profilePictureUrl, newPassword }, context) => {
  if (!context.user) { throw new HttpError(403) }

  // TODO: Nicer error handling! Right now everything is returned as 500 while it could be instead
  //   useful error message about username being taken / not unique, and other validation errors.
  try {
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
  } catch (e) {
   if (isPrismaError(e)) {
      throw prismaErrorToHttpError(e)
    } else {
      throw e
    }
  }
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
