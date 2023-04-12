import { createThought } from './actions.js'

export const devSeedBasic = async (prismaClient) => {
    const user = await createUser(prismaClient, {
        username: "RiuTheDog",
        password: "bark1234"
    })

    await createThought(
        { tagNames: ["animals.cats"], textMarkdown: "## My favorite cats\n  - Kira\n  - Garfield" },
        { user, entities: { Thought: prismaClient.thought } }
    )
}

async function createUser (prismaClient, data) {
    const { password, ...newUser } = await prismaClient.user.create({ data })
    return newUser
}