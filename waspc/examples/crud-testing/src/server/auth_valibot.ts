import { defineAdditionalSignupFields } from '@wasp/auth/index.js'
import { parse, string } from 'valibot'

export const fields = defineAdditionalSignupFields({
  address: (data) => {
    const AddressSchema = string('Address is required', [
      (input) => {
        if (input.length < 5) {
          return {
            issue: {
              validation: 'custom',
              message: 'Address must be at least 5 characters long',
              input,
            }
          }
        }
        return {
          output: input,
        }
      }
    ])
    return parse(AddressSchema, data.address)
  },
})


