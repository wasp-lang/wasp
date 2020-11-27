import HttpError from '@wasp/core/HttpError.js'

export const getUser = async ({ username }, context) => {
  // TODO: Do some error handling?
  const user = await context.entities.User.findUnique({
    where: { username },
    // TODO: Tricky, if you forget this you could return unwanted fields
    //   like hashed password!
    //   It would be cool if we had some protection against making this mistake easily.
    select: {
      ...userPublicSelection,
      followedBy: { select: { id: true } }
    }
  })
  if (!user) throw new HttpError(404, 'No user with username ' + username)

  userSetFollowedFields(user, context.user)

  return user
}

export const userPublicSelection = {
  id: true,
  username: true,
  email: true,
  bio: true,
  profilePictureUrl: true
}

const userSetFollowedFields = (user, me) => {
  user.following = me && user.followedBy.find(({ id }) => id === me.id)
  user.followersCount = user.followedBy.length
  delete user.followedBy
}
