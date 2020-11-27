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

const userSetFollowedFields = (user, me) => {
  user.following = me && user.followedBy.find(({ id }) => id === me.id)
  user.followersCount = user.followedBy.length
  delete user.followedBy
}

// TODO: I extracted this articleInclude and articleSetFavoritedFields to enable
//   reusing of logic that shapes articles as they come out of the server,
//   but I wonder if there is a more elegant way - here there are a lot of assumptions,
//   and it is easy to not use where it should be used or use it in wrong setting.

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

const getArticles = async ({ where }, context) => {
  // TODO: Do some error handling?
  const articles = await context.entities.Article.findMany({
    where,
    include: articleInclude
  })

  for (const article of articles) {
    articleSetFavoritedFields(article, context.user)
  }

  return articles
}

export const getArticlesByUser = async ({ username }, context) => {
  return await getArticles({ where: { user: { username } } }, context)
}

export const getFavoritedArticles = async (args, context) => {
  if (!context.user) { throw new HttpError(403) }
  return await getArticles({
    where: { favoritedBy: { some: { username: context.user.username } } },
  }, context)
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

export const getArticleComments = async ({ slug }, context) => {
  // TODO: Do some error handling?
  const comments = await context.entities.Comment.findMany({
    where: { article: { slug } },
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
