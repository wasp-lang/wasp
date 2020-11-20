import HttpError from '@wasp/core/HttpError.js'

export const getUser = async ({ username }, context) => {
  // TODO: Do some error handling?
  const user = await context.entities.User.findOne({ where: { username } })
  if (!user) throw new HttpError(404, 'No user with username ' + username)
  return user
}

export const getArticlesByUser = async ({ username }, context) => {
  // TODO: Do some error handling?
  const articles = await context.entities.Article.findMany({
    where: {
      user: { username }
    }
  })
  return articles
}

export const getArticle = async ({ id }, context) => {
  // TODO: Do some error handling?
  const article = await context.entities.Article.findOne({
    where: { id }
  })
  return article
}
