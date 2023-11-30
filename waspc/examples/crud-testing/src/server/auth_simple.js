import { defineAdditionalSignupFields } from 'wasp/auth/index.js'

export const fields = defineAdditionalSignupFields({
  address: (data) => data.address,
})
