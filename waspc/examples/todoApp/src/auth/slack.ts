import { defineUserSignupFields } from 'wasp/server/auth'

export function config() {
  console.log('Inside user-supplied Slack config')
  return {
    scopes: [],
  }
}

export const userSignupFields = defineUserSignupFields({})
