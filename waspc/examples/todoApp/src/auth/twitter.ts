import { defineUserSignupFields } from 'wasp/server/auth'

export function config() {
  console.log('Inside user-supplied Twitter config')
  return {
    scopes: ['users.read', 'tweet.read'],
  }
}

export const userSignupFields = defineUserSignupFields({})
