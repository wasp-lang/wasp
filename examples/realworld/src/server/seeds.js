import { createArticle } from './article/actions.js'

export const devSeedSimple = async (prismaClient) => {
  const user = await createUser(prismaClient, {
      username: "RiuTheDog",
      password: "bark1234",
      email: "riu@wasp-lang.dev"
  })

  await createArticle(
    {
        title: "The Art of Barking",
        description: "Elevate your barking to another level by following advice of a master barkerer Riu",
        markdownContent: (
          "# Barking - what is it really?"
          + "\n\nThe fact that wolves don't bark but dogs do already tells us a lot about barking: "
          + "it can't be studied without also studying humans and their influence on dogs."
          + "\n\nTODO: bark bark bark"
        ),
        tags: [{ name: 'barking' }, { name: 'dogs' }]
    },
    { user, entities: { Article: prismaClient.article } }
  )
}

async function createUser (prismaClient, data) {
  const { password, ...newUser } = await prismaClient.user.create({ data })
  return newUser
} 