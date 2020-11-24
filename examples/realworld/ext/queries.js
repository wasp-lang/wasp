import HttpError from '@wasp/core/HttpError.js'

const userPublicSelection = {
  id: true,
  username: true,
  email: true,
  bio: true,
  profilePictureUrl: true
}

export const getUser = async ({ username }, context) => {
  // TODO: Do some error handling?
  const user = await context.entities.User.findOne({
    where: { username },
    // TODO: Tricky, if you forget this you could return unwanted fields
    //   like hashed password!
    //   It would be cool if we had some protection against making this mistake easily.
    select: userPublicSelection
  })
  if (!user) throw new HttpError(404, 'No user with username ' + username)
  return user
}

export const getArticlesByUser = async ({ username }, context) => {
  // TODO: Do some error handling?
  const articles = await context.entities.Article.findMany({
    where: { user: { username } }
  })
  return articles
}

export const getArticle = async ({ id }, context) => {
  // TODO: Do some error handling?
  const article = await context.entities.Article
    .findOne({
      where: { id },
      include: {
        user: {
          // TODO: Tricky, if you forget this you could return unwanted fields
          //   like hashed password!
          //   It would be cool if we had some protection against making this mistake easily.
          select: userPublicSelection
        }
      }
    })
  return article
}
