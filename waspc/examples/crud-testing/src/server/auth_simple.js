import { defineUserSignupFields } from 'wasp/auth/index.js'

export const userSignupFields = defineUserSignupFields({
  address: (data) => data.address,
})
