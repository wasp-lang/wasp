import HttpError from '@wasp/core/HttpError.js'
import AuthError from '@wasp/core/AuthError.js'
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
    if (e instanceof AuthError) {
      throw new HttpError(422, 'Validation failed', { message: e.message })
    } else if (isPrismaError(e)) {
      throw prismaErrorToHttpError(e)
    } else {
      throw new HttpError(500)
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
