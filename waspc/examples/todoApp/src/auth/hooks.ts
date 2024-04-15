import { defineHook } from 'wasp/server/auth'

export const postLoginHook = defineHook('postLogin', async (providerId) => {
  // Do something after the user logs in.
})

export const postSingupHook = defineHook('postSignup', async (providerId) => {
  // Do something after the user signs up.
})
