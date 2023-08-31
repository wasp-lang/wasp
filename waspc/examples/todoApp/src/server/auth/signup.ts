import { defineAdditionalSignupFields } from '@wasp/auth/index.js'

export const fields = defineAdditionalSignupFields({
  address: {
    get: (data) => data.address as string | undefined,
    validate: (value) => {
      if (!value || value.length < 5) {
        throw new Error('Address is required')
      }
    },
  },
})
