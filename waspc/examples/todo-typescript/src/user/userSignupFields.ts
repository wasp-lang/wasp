import { defineUserSignupFields } from 'wasp/server/auth'

export const googleUserSignupFields = defineUserSignupFields({
  address: () => 'Placeholder address',
})

export const githubUserSignupFields = defineUserSignupFields({
  address: () => 'Placeholder address',
})

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (typeof data.address !== 'string') {
      throw new Error('Address must be provided on signup.')
    }
    return data.address
  },
})
