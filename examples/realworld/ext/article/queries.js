import HttpError from '@wasp/core/HttpError.js'

import { userPublicSelection } from '../user/queries.js'

// TODO: I extracted this articleInclude and articleSetFavoritedFields to enable
//   reusing of logic that shapes articles as they come out of the server,
//   but I wonder if there is a more elegant way - here there are a lot of assumptions,
//   and it is easy to not use where it should be used or use it in a wrong setting.

const articleInclude = {
  user: {
    // TODO: Tricky, if you forget this you could return unwanted fields
    //   like hashed password!
    //   It would be cool if we had some protection against making this mistake easily.
    select: userPublicSelection
  },
  tags: true,
  favoritedBy: {
    select: {
      // TODO: Tricky, if I forgot this select here, sensitive data could leak out (hashed password).
      username: true
    }
  }
}

const articleSetFavoritedFields = (article, user) => {
  article.favorited = user && article.favoritedBy.find(f => f.username === user.username)
  article.favoritesCount = article.favoritedBy.length
  delete article.favoritedBy
}

const getArticles = async (queryArgs, context) => {
  // TODO: Do some error handling?
  const articles = await context.entities.Article.findMany({
    ...queryArgs,
    include: articleInclude
  })

  for (const article of articles) {
    articleSetFavoritedFields(article, context.user)
  }

  return articles
}

export const getArticlesByUser = async ({ username, skip, take }, context) => {
  const where = { user: { username } }
  const articles = await getArticles({ where, skip, take }, context)
  const count = await context.entities.Article.count({ where })
  return { articles, count }
}

export const getFavoritedArticles = async ({ username, skip, take }, context) => {
  if (!context.user) { throw new HttpError(403) }
  const where = { favoritedBy: { some: { username: context.user.username } } }
  const articles = await getArticles({ where, skip, take }, context)
  const count = await context.entities.Article.count({ where })
  return { articles, count }
}

export const getFollowedArticles = async ({ skip, take }, context) => {
  if (!context.user) { throw new HttpError(403) }

  const followedUsersIds = (await context.entities.User.findUnique({
    where: { id: context.user.id },
    include: { following: { select: { id: true } } }
  })).following.map(({ id }) => id)

  const where = { user: { id: { in: followedUsersIds } } }
  const articles = await getArticles({ where, skip, take }, context)
  const count = await context.entities.Article.count({ where })
  return { articles, count }
}

export const getAllArticles = async ({ skip, take }, context) => {
  const articles = await getArticles({ skip, take }, context)
  const count = await context.entities.Article.count()
  return { articles, count }
}

export const getArticle = async ({ slug }, context) => {
  // TODO: Do some error handling?
  const article = await context.entities.Article.findUnique({
    where: { slug },
    include: articleInclude
  })
  articleSetFavoritedFields(article, context.user)
  return article
}

export const getArticleComments = async ({ articleId }, context) => {
  // TODO: Do some error handling?
  const comments = await context.entities.Comment.findMany({
    where: { articleId },
    include: {
      user: {
        // TODO: Tricky, if you forget this you could return unwanted fields
        //   like hashed password!
        //   It would be cool if we had some protection against making this mistake easily.
        select: userPublicSelection
      }
    }
  })
  return comments
}

export const getTags = async (_args, context) => {
  const tags = await context.entities.ArticleTag.findMany()
  // NOTE: This is expensive!
  //   Consider using a time-limited cache or do some other trick to make it less expensive.
  for (const tag of tags) {
    tag.numArticles = await context.entities.Article.count({ where: { tags: { some: { name: tag.name }}}})
  }
  return tags
}
